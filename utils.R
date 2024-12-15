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

# Generate Code 128 barcode PDF
export_barcode_pdf <- function(sku, page_width, page_height, unit = "in") {
  # Create a temporary file path for the PDF
  temp_dir <- tempdir() 
  if (unit == "cm") {
    # 1 cm = 0.393701 in 
    page_width <- page_width / 2.54  
    page_height <- page_height / 2.54
  }
  
  if(length(unique(sku)) > 1)
    pdf_path <- paste0(temp_dir, "/multiple_barcode")  # 组合文件夹路径和文件名
  
  if(length(unique(sku)) == 1)
    pdf_path <- paste0(temp_dir, "/", unique(sku), "_", length(sku), "_barcode")
  
  custom_create_PDF(Labels = sku, 
                    name = pdf_path, 
                    type = "linear", 
                    page_width = page_width, 
                    page_height = page_height,
                    numrow = 1, 
                    numcol = 1, 
                    width_margin = 0, 
                    height_margin = 0.05)
  
  return(paste0(pdf_path, ".pdf"))
}

# Save compressed image to the server
save_compressed_image <- function(file_path, output_dir, image_name, quality = 75) {
  # Validate inputs
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("Invalid input file path.")
  }
  
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist.")
  }
  
  print(paste("Saving image to:", output_dir))
  
  tryCatch({
    # Load the image
    img <- magick::image_read(file_path)
    
    # Compress the image
    compressed_img <- magick::image_scale(img, "500x")  # Resize
    compressed_img <- magick::image_convert(compressed_img, format = "jpeg")
    
    # Save the compressed image
    output_path <- file.path(output_dir, image_name)
    magick::image_write(compressed_img, path = output_path, quality = quality)
    
    return(output_path)  # Return the saved image path
  }, error = function(e) {
    return(NULL)
  })
}


# Generate unique code
generate_unique_code <- function(input, length = 4) {
  # Validate input
  if (is.null(input) || input == "") {
    return(NULL)  
  }
    
  # Validate length parameter
  if (!is.numeric(length) || length <= 0) {
    stop("Length must be a positive integer.")
  }

  # Generate a hash value
  hash_value <- digest::digest(enc2utf8(input), algo = "sha512")
  
  # Extract numeric seed from the hash
  hash_numeric <- abs(sum(utf8ToInt(hash_value))) %% .Machine$integer.max
  
  set.seed(hash_numeric)
  
  # Generate a random alphanumeric code
  random_output <- paste0(sample(c(LETTERS, 0:9), length, replace = TRUE), collapse = "")
  return(random_output)
}

# Generate SKU
generate_sku <- function(item_type_data, major_type, minor_type, item_name, maker) {
  if (is.null(major_type) || major_type == "" || 
      is.null(minor_type) || minor_type == "" || 
      is.null(item_name) || item_name == "" || 
      is.null(maker) || maker == "") {
    return("")  # Return empty if any input is invalid
  }
  
  # Get MajorTypeSKU and MinorTypeSKU
  major_type_sku <- item_type_data %>%
    filter(MajorType == major_type) %>%
    pull(MajorTypeSKU) %>%
    unique()
  
  minor_type_sku <- item_type_data %>%
    filter(MinorType == minor_type) %>%
    pull(MinorTypeSKU) %>%
    unique()
  
  if (length(major_type_sku) == 0 || length(minor_type_sku) == 0) {
    return("")  # Return empty if no matching SKUs are found
  }
  
  # Generate unique code
  unique_code <- generate_unique_code(paste(item_name, maker, sep = "_"), length = 4)
  
  # Create the SKU in the format: MajorTypeSKU-MinorTypeSKU-UniqueCode
  paste0(major_type_sku, "-", minor_type_sku, "-", unique_code)
}

# Remove tone of letters
remove_tone <- function(text) {
  # 替换规则：音调字母 -> 无音调字母
  text <- stri_replace_all_regex(text, "ā|á|ǎ|à|a", "a")
  text <- stri_replace_all_regex(text, "ē|é|ě|è|e", "e")
  text <- stri_replace_all_regex(text, "ī|í|ǐ|ì|i", "i")
  text <- stri_replace_all_regex(text, "ō|ó|ǒ|ò|o", "o")
  text <- stri_replace_all_regex(text, "ū|ú|ǔ|ù|u", "u")
  text <- stri_replace_all_regex(text, "ǖ|ǘ|ǚ|ǜ|ü", "u")
  text <- stri_replace_all_regex(text, "Ā|Á|Ǎ|À|A", "A")
  text <- stri_replace_all_regex(text, "Ē|É|Ě|È|E", "E")
  text <- stri_replace_all_regex(text, "Ī|Í|Ǐ|Ì|I", "I")
  text <- stri_replace_all_regex(text, "Ō|Ó|Ǒ|Ò|O", "O")
  text <- stri_replace_all_regex(text, "Ū|Ú|Ǔ|Ù|U", "U")
  text <- stri_replace_all_regex(text, "Ǖ|Ǘ|Ǚ|Ǜ|Ü", "U")
  return(text)
}

# Define an empty inventory template
create_empty_inventory <- function() {
  data.frame(
    SKU = character(),            # Product SKU
    Maker = character(),          # Supplier
    MajorType = character(),      # Major category
    MinorType = character(),      # Minor category
    ItemName = character(),       # Item name
    Quantity = numeric(),         # Quantity in stock
    ProductCost = numeric(),      # Product Cost
    ShippingCost = numeric(),     # Shipping Cost
    ItemImagePath = character(),  # Path to item image
    stringsAsFactors = FALSE      # Avoid factor columns
  )
}

# Map column names and filter only mapped columns
map_column_names <- function(data, column_mapping) {
  # Get the mapped columns in the order of column_mapping
  mapped_columns <- names(column_mapping)[names(column_mapping) %in% names(data)]
  
  # If no matching columns, return an empty data frame
  if (length(mapped_columns) == 0) {
    return(data.frame())
  }
  
  # Select and reorder columns in the order of column_mapping
  data <- data[, mapped_columns, drop = FALSE]
  
  # Rename columns according to column_mapping
  data <- setNames(data, column_mapping[mapped_columns])
  
  return(data)
}

# Function to render the image column (local images with public URL prefix)
render_image_column <- function(image_column_data, 
                                host_url,  # 不使用默认值，确保明确传入
                                placeholder = "https://dummyimage.com/50x50/cccccc/000000.png&text=No+Image") {
  # 验证输入参数是否正确
  if (missing(host_url) || is.null(host_url)) {
    stop("Error: 'host_url' must be provided and cannot be NULL.")
  }
  
  sapply(image_column_data, function(img) {
    if (is.na(img) || img == "") {
      # 返回占位符图片
      paste0('<img src="', placeholder, '" loading="lazy" width="50" height="50" style="object-fit:cover;"/>')
    } else {
      # 拼接完整的图片 URL
      img_path <- paste0(host_url, "/images/", basename(img))
      paste0('<img src="', img_path, '" loading="lazy" width="50" height="50" style="object-fit:cover;"/>')
    }
  }, USE.NAMES = FALSE)
}

# Function to render datatable with images
render_table_with_images <- function(data, 
                                     column_mapping, 
                                     image_column = NULL,
                                     options = list()) {
  if (!is.null(image_column) && nrow(data) > 0) {
    # Render the image column
    data[[image_column]] <- render_image_column(data[[image_column]], host_url)
  }
  
  # Map column names for user-friendly display
  if (!is.null(column_mapping)) {
    data <- map_column_names(data, column_mapping)
  }
  
  # Return the rendered datatable
  datatable(
    data,
    escape = FALSE,  # Disable HTML escaping to allow rendering of images
    selection = 'single',
    filter = "top",
    rownames = FALSE,
    options = options
  )
}


update_status <- function(con, unique_id, new_status, defect_status = NULL, refresh_trigger = NULL) {
  if (!new_status %in% names(status_columns)) {
    stop("Invalid status provided")
  }
  
  # 获取时间戳列
  timestamp_column <- status_columns[[new_status]]
  
  # 动态生成 SQL 查询
  if (!is.null(defect_status)) {
    query <- paste0(
      "UPDATE unique_items SET Status = ?, ", 
      timestamp_column, " = NOW(), Defect = ? WHERE UniqueID = ?"
    )
    params <- list(new_status, defect_status, unique_id)
  } else {
    query <- paste0(
      "UPDATE unique_items SET Status = ?, ", 
      timestamp_column, " = NOW() WHERE UniqueID = ?"
    )
    params <- list(new_status, unique_id)
  }
  
  # 执行 SQL 更新
  dbExecute(con, query, params = params)
  
  # 触发刷新
  if (!is.null(refresh_trigger)) {
    refresh_trigger(!refresh_trigger())
  }
}



get_inventory_overview <- function(con, sku) {
  query <- "
    SELECT 
      Status, 
      Defect, 
      COUNT(*) as Count
    FROM 
      unique_items
    WHERE 
      SKU = ?
    GROUP BY 
      Status, Defect"
  
  # 查询数据库
  inventory_data <- dbGetQuery(con, query, params = list(sku))
  
  # 初始化结果结构
  overview <- list(
    domestic_instock = list(defect = 0, repaired = 0, pristine = 0),
    in_transit = list(defect = 0, repaired = 0, pristine = 0),
    us_instock = list(defect = 0, repaired = 0, pristine = 0)
  )
  
  # 根据查询结果填充数据
  for (i in seq_len(nrow(inventory_data))) {
    row <- inventory_data[i, ]
    
    if (row$Status == "国内入库") {
      if (row$Defect == "瑕疵") {
        overview$domestic_instock$defect <- row$Count
      } else if (row$Defect == "修复") {
        overview$domestic_instock$repaired <- row$Count
      } else {
        overview$domestic_instock$pristine <- row$Count
      }
    } else if (row$Status %in% c("国内出库", "国内售出")) {
      if (row$Defect == "瑕疵") {
        overview$in_transit$defect <- overview$in_transit$defect + row$Count
      } else if (row$Defect == "修复") {
        overview$in_transit$repaired <- overview$in_transit$repaired + row$Count
      } else {
        overview$in_transit$pristine <- overview$in_transit$pristine + row$Count
      }
    } else if (row$Status == "美国入库") {
      if (row$Defect == "瑕疵") {
        overview$us_instock$defect <- row$Count
      } else if (row$Defect == "修复") {
        overview$us_instock$repaired <- row$Count
      } else {
        overview$us_instock$pristine <- row$Count
      }
    }
  }
  
  return(overview)
}

# 提取公共方法：获取 SKU 数据
fetchSkuData <- function(sku, con) {
  # 查询 SKU 的基本信息和相关状态数据
  query <- "
    SELECT 
      inv.ItemImagePath,
      inv.ItemName,
      inv.Maker,
      inv.MajorType,
      inv.MinorType,
      inv.Quantity AS TotalQuantity, -- 总库存数量
      SUM(CASE WHEN u.Status = '采购' THEN 1 ELSE 0 END) AS PendingQuantity, -- 待入库数
      SUM(CASE WHEN u.Status = '国内入库' AND u.Defect != '瑕疵' THEN 1 ELSE 0 END) AS AvailableForOutbound, -- 可出库数
      SUM(CASE WHEN u.Status = '国内入库' AND u.Defect != '瑕疵' THEN 1 ELSE 0 END) AS AvailableForSold -- 可售出数
    FROM inventory AS inv
    LEFT JOIN unique_items AS u
      ON inv.SKU = u.SKU
    WHERE inv.SKU = ?
    GROUP BY inv.ItemImagePath, inv.ItemName, inv.Maker, inv.MajorType, inv.MinorType, inv.Quantity
  "
  
  # 执行查询并返回结果
  dbGetQuery(con, query, params = list(sku))
}


renderItemInfo <- function(output, output_name, item_info, img_path, count_label = "待入库数", count_field = "PendingQuantity") {
  # 如果 item_info 为空或没有数据，构造一个默认空数据框
  if (is.null(item_info) || nrow(item_info) == 0) {
    item_info <- data.frame(
      ItemName = "",
      Maker = "",
      MajorType = "",
      MinorType = "",
      PendingQuantity = 0,
      AvailableForOutbound = 0,
      AvailableForSold = 0,
      stringsAsFactors = FALSE
    )
  }
  
  # 动态获取数量值
  count_value <- item_info[[count_field]][1]
  
  # 动态渲染 UI
  output[[output_name]] <- renderUI({
    fluidRow(
      column(
        4,
        div(
          style = "text-align: center;",
          img(
            src = img_path,
            height = "300px",
            style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);"
          )
        )
      ),
      column(
        8,
        div(
          style = "padding: 20px; background-color: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 8px;
                             box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); height: 100%;",
          tags$h4(
            "商品信息",
            style = "border-bottom: 3px solid #4CAF50; margin-bottom: 15px; padding-bottom: 8px; font-weight: bold; color: #333;"
          ),
          tags$table(
            style = "width: 100%; font-size: 16px; color: #444;",
            tags$tr(
              tags$td(tags$strong("商品名:"), style = "padding: 8px 10px; width: 120px; vertical-align: top;"),
              tags$td(tags$span(item_info$ItemName[1], style = "color: #4CAF50; font-weight: bold;"))
            ),
            tags$tr(
              tags$td(tags$strong("供应商:"), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(item_info$Maker[1], style = "color: #4CAF50;"))
            ),
            tags$tr(
              tags$td(tags$strong("大类:"), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(item_info$MajorType[1], style = "color: #4CAF50;"))
            ),
            tags$tr(
              tags$td(tags$strong("小类:"), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(item_info$MinorType[1], style = "color: #4CAF50;"))
            ),
            tags$tr(
              tags$td(tags$strong(count_label), style = "padding: 8px 10px; vertical-align: top;"),
              tags$td(tags$span(
                ifelse(count_value == 0, paste0("无", count_label), count_value),
                style = "color: #FF4500; font-weight: bold;"
              ))
            )
          )
        )
      )
    )
  })
}

handleOperation <- function(
    operation_name, # 操作名称（入库、出库、售出）
    sku_input,      # SKU 输入字段
    output_name,    # 输出的 UI 名称
    query_status,   # 查询的初始状态
    update_status_value, # 更新后的状态
    count_label,    # 显示的计数标签
    count_field,    # 计数字段名称
    con,            # 数据库连接
    output,         # 输出对象
    refresh_trigger, # 数据刷新触发器
    session         # 当前会话对象
) {
  sku <- trimws(sku_input) # 清理空格
  
  if (is.null(sku) || sku == "") {
    showNotification(paste0("请先扫描 SKU！"), type = "error")
    renderItemInfo(output, output_name, NULL, placeholder_300px_path, count_label, count_field)
    return()
  }
  
  tryCatch({
    # 查询符合条件的物品
    sku_items <- dbGetQuery(con, paste0("
      SELECT UniqueID 
      FROM unique_items 
      WHERE SKU = ? AND Status = '", query_status, "' AND Defect != '瑕疵'
      LIMIT 1"), 
                            params = list(sku))
    
    if (nrow(sku_items) == 0) {
      showNotification(paste0("无可", operation_name, "的物品，所有该商品已完成 ", operation_name, "！"), type = "message")
      return()
    }
    
    # 更新状态
    update_status(
      con = con,
      unique_id = sku_items$UniqueID[1],
      new_status = update_status_value,
      refresh_trigger = refresh_trigger
    )
    
    # 成功提示
    showNotification(paste0("物品成功", operation_name, "！"), type = "message")
    
    # 查询 SKU 数据并刷新 UI
    item_info <- fetchSkuData(sku, con)
    
    renderItemInfo(
      output = output,
      output_name = output_name,
      item_info = item_info,
      img_path = ifelse(
        is.na(item_info$ItemImagePath[1]),
        placeholder_300px_path,
        paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))
      ),
      count_label = count_label,
      count_field = count_field
    )
    
    # 如果计数字段为 0，显示模态弹窗
    if (item_info[[count_field]][1] == 0) {
      showModal(modalDialog(
        title = paste0(operation_name, "完成"),
        paste0("此 SKU 的商品已全部完成 ", operation_name, "！"),
        easyClose = TRUE,
        footer = modalButton("确定")
      ))
    }
    
    # 清空输入框
    updateTextInput(session, paste0(operation_name, "_sku"), value = "")
    if (operation_name == "入库") {
      updateCheckboxInput(session, "defective_item", value = FALSE)
    }
    
  }, error = function(e) {
    # 错误处理
    showNotification(paste0(operation_name, "失败：", e$message), type = "error")
  })
}

