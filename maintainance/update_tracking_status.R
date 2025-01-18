library(DBI)
library(httr)

# Connect to backend database
db_connection <- function() {
  dbConnect(
    RMariaDB::MariaDB(),
    dbname = "inventory_system",
    host = "localhost",
    user = "root",
    password = "goldenbeanllc",
    encoding = "utf8mb4"
  )
}

### USPS API functions

# 获取 Access Token
get_access_token <- function(client_id, client_secret) {
  # Token 文件路径
  token_file <- "/srv/shiny-server/inventory/data/token_data.rds"
  
  # 检查是否有已存储的 Token
  if (file.exists(token_file)) {
    token_data <- readRDS(token_file)
    # 检查 Token 是否有效
    if (Sys.time() < token_data$expiry) {
      message("Using cached token.")
      return(token_data$token)
    }
  }
  
  # 如果没有 Token 或已过期，重新请求
  response <- httr::POST(
    url = "https://apis.usps.com/oauth2/v3/token",
    body = list(
      grant_type = "client_credentials",
      client_id = client_id,
      client_secret = client_secret
    ),
    encode = "form"
  )
  
  if (httr::status_code(response) == 200) {
    token_data <- httr::content(response, as = "parsed")
    access_token <- token_data$access_token
    expiry <- Sys.time() + as.numeric(token_data$expires_in) - 60  # 提前 1 分钟失效
    
    # 存储 Token 和过期时间
    saveRDS(list(token = access_token, expiry = expiry), token_file)
    message("New token requested and saved.")
    return(access_token)
  } else {
    stop("Failed to get access token: ", httr::content(response, as = "text"))
  }
}

# 查询Tracking状态函数
get_tracking_info <- function(tracking_number, access_token, request_counter, request_timestamp) {
  if (is.null(tracking_number) || tracking_number == "") {
    stop("Invalid tracking number.")
  }
  
  if (request_counter >= 30 && Sys.time() < request_timestamp + lubridate::hours(1)) {
    stop("Hourly request limit reached. Please wait.")
  }
  
  url <- paste0("https://apis.usps.com/tracking/v3/tracking/", tracking_number, "?expand=SUMMARY")
  response <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))
  
  # 更新请求计数器和时间戳
  if (Sys.time() >= request_timestamp + lubridate::hours(1)) {
    request_counter <- 1
    request_timestamp <- Sys.time()
  } else {
    request_counter <- request_counter + 1
  }
  
  if (httr::status_code(response) == 200) {
    return(list(
      content = httr::content(response, as = "parsed"),
      request_counter = request_counter,
      request_timestamp = request_timestamp
    ))
  } else {
    warning("Tracking request failed: ", httr::content(response, as = "text"))
    return(NULL)
  }
}

# 更新订单状态
update_order_status <- function(order_id, new_status, con) {
  dbExecute(
    con,
    "UPDATE orders SET OrderStatus = ?, updated_at = CURRENT_TIMESTAMP WHERE OrderID = ?",
    params = list(new_status, order_id)
  )
}

# 状态映射规则
extract_latest_status <- function(eventSummaries) {
  # 检查是否有 eventSummaries
  if (is.null(eventSummaries) || length(eventSummaries) == 0) {
    return(NA)
  }
  
  # 提取第一条记录
  latest_event <- eventSummaries[[1]]
  
  if (is.null(latest_event) || latest_event == "") {
    return(NA)
  }
  
  # 匹配状态
  status <- dplyr::case_when(
    grepl("Pre-Shipment|USPS Awaiting Item|Shipping Label Created,", latest_event) ~ "装箱",
    grepl("USPS picked up item|USPS in possession of item|Departed Post Office", latest_event) ~ "发出",
    grepl("In Transit|Departed|Arrived at|Your item arrived at|Your item departed|is moving", latest_event) ~ "在途",
    grepl("Your item was delivered", latest_event) ~ "送达"
  )
  
  return(status)
}

# 订单状态更新主逻辑
update_tracking_status <- function() {
  
  message(Sys.time())
  
  # 数据库连接信息
  con <- db_connection()
  
  # USPS API credentials
  client_id <<- Sys.getenv("USPS_CLIENT_ID")
  client_secret <<- Sys.getenv("USPS_CLIENT_SECRET")
  
  # 获取 Access Token
  token <- get_access_token(client_id, client_secret)
  
  # 初始化计数器和时间戳
  request_counter <- 0
  request_timestamp <- Sys.time()
  
  # 查询需要更新的订单
  eligible_orders <- dbGetQuery(con, "
    SELECT OrderID, UsTrackingNumber, OrderStatus, updated_at
    FROM orders
    WHERE OrderStatus IN ('装箱', '发出', '在途')
      AND (updated_at IS NULL OR TIMESTAMPDIFF(HOUR, updated_at, NOW()) >= 8)
  ")
  
  if (nrow(eligible_orders) == 0) {
    message("No eligible orders for update.")
    dbDisconnect(con)
    return()
  }
  
  message(nrow(eligible_orders), " packages to be updated")
  
  # 遍历符合条件的订单
  for (i in 1:nrow(eligible_orders)) {
    order <- eligible_orders[i, ]
    message("Updating tracking number: ", order$UsTrackingNumber)
    tracking_result <- get_tracking_info(order$UsTrackingNumber, token, request_counter, request_timestamp)
    
    if (!is.null(tracking_result)) {
      request_counter <- tracking_result$request_counter
      request_timestamp <- tracking_result$request_timestamp
      new_status <- extract_latest_status(tracking_result$content$eventSummaries)
      
      if(is.na(new_status))
      {
        message("NA Status: ", tracking_result$content$eventSummaries)
      } else {
        message(order$OrderStatus, " --> ", new_status)
        update_order_status(order$OrderID, new_status, con)
      }
    } else {
      message("No tracking result returned!")
    }
    message("-------------------------------------------")
  }
  dbDisconnect(con)
}

### 

update_tracking_status()