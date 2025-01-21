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

# 日志记录函数
log_message <- function(msg) {
  log_file <- "/var/log/update_dhl_tracking_status.log"
  
  if (!file.exists(log_file)) {
    dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
    file.create(log_file)
  }
  
  cat(sprintf("[%s] %s\n", Sys.time(), msg), file = log_file, append = TRUE)
}

# 查询 DHL Tracking 信息
get_dhl_tracking_info <- function(tracking_number) {
  base_url <- "https://api-eu.dhl.com/track/shipments"
  api_key <- Sys.getenv("DHL_API_KEY")
  
  if (is.null(tracking_number) || tracking_number == "") {
    stop("Invalid tracking number.")
  }
  
  response <- tryCatch({
    httr::GET(
      url = base_url,
      httr::add_headers(`DHL-API-Key` = api_key),
      query = list(trackingNumber = tracking_number, language = "en")
    )
  }, error = function(e) {
    stop("DHL 请求失败：", e$message)
  })
  
  if (httr::status_code(response) == 200) {
    log_message(paste("DHL Tracking info retrieved for:", tracking_number))
    return(httr::content(response, as = "parsed"))
  } else {
    warning("DHL API 请求失败：", httr::content(response, as = "text"))
    return(NULL)
  }
}

extract_dhl_status <- function(events) {
  if (is.null(events) || length(events) == 0) {
    return(NA)
  }
  
  latest_event <- events[[1]]
  
  if (is.null(latest_event) || is.null(latest_event$description)) {
    return(NA)
  }
  
  # 根据 DHL 返回的事件描述更新状态
  status <- dplyr::case_when(
    grepl("Shipment picked up", latest_event$description) ~ "包裹发出",
    grepl("In Transit", latest_event$description) ~ "在途运输",
    grepl("Clearance processing complete", latest_event$description) ~ "美国清关",
    grepl("Delivered", latest_event$description) ~ "包裹送达",
    TRUE ~ "未知"
  )
  
  return(status)
}

update_dhl_tracking_status <- function() {
  log_message("DHL Tracking status update started.")
  
  con <- db_connection()
  
  # 获取 API Key
  api_key <- Sys.getenv("DHL_API_KEY")
  
  # 查询需要更新状态的运单
  eligible_shipments <- dbGetQuery(con, "
    SELECT TrackingNumber, Status, UpdatedAt 
    FROM intl_shipments
    WHERE Status IN ('包裹发出', '在途运输', '美国清关')
      AND (UpdatedAt IS NULL OR TIMESTAMPDIFF(HOUR, UpdatedAt, NOW()) >= 8)
  ")
  
  if (nrow(eligible_shipments) == 0) {
    log_message("No eligible DHL shipments for update.")
    dbDisconnect(con)
    return()
  }
  
  for (i in 1:nrow(eligible_shipments)) {
    shipment <- eligible_shipments[i, ]
    tracking_number <- shipment$TrackingNumber
    log_message(paste("Updating tracking number:", tracking_number))
    
    # 获取 DHL 跟踪信息
    tracking_result <- get_dhl_tracking_info(tracking_number, api_key)
    
    if (!is.null(tracking_result)) {
      # 提取最新状态
      new_status <- extract_dhl_status(tracking_result$events)
      
      if (!is.na(new_status) && new_status != shipment$Status) {
        # 更新数据库中的状态
        dbExecute(
          con,
          "UPDATE intl_shipments 
           SET Status = ?, UpdatedAt = CURRENT_TIMESTAMP 
           WHERE TrackingNumber = ?",
          params = list(new_status, tracking_number)
        )
        log_message(paste("Updated status for:", tracking_number, "to:", new_status))
      } else {
        log_message(paste("No status update needed for:", tracking_number))
      }
    } else {
      log_message(paste("Failed to retrieve tracking info for:", tracking_number))
    }
  }
  
  dbDisconnect(con)
  log_message("DHL Tracking status update completed.")
}


