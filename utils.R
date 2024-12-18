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

# Get image height width ratio
get_image_dimensions <- function(image_path) {
  img <- magick::image_read(image_path)
  info <- magick::image_info(img)
  list(width = info$width, height = info$height)
}

# Save compressed image to the server
save_compressed_image <- function(file_path, output_dir, image_name, quality = 75, max_width = 500) {
  # 验证输入
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("Invalid input file path.")
  }
  
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist.")
  }
  
  tryCatch({
    # 加载图片
    img <- magick::image_read(file_path)
    
    # 获取原始宽度
    img_info <- magick::image_info(img)
    original_width <- img_info$width
    
    # 判断是否需要缩放
    if (original_width > max_width) {
      img <- magick::image_scale(img, paste0(max_width, "x"))  # 缩放图片
    }
    
    # 转为 JPEG 格式
    img <- magick::image_convert(img, format = "jpeg")
    
    # 保存图片
    output_path <- file.path(output_dir, image_name)
    magick::image_write(img, path = output_path, quality = quality)
    
    return(output_path)
  }, error = function(e) {
    showNotification(paste("图片压缩失败:", e$message), type = "error")
    return(NULL)
  })
}

# 将 Base64 编码的图片数据解码并保存为实际图片文件
base64_decode_image <- function(base64_string, output_path) {
  # 提取 Base64 数据部分（去掉头部信息，如 "data:image/png;base64,"）
  base64_data <- gsub("^data:image/[^;]+;base64,", "", base64_string)
  
  # 解码 Base64 数据为二进制文件
  decoded_image <- base64enc::base64decode(base64_data)
  
  # 写入文件
  writeBin(decoded_image, output_path)
}

render_image_preview <- function(img_src, img_info, ns) {
  renderUI({
    div(
      tags$img(src = img_src, height = "200px",
               style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 10px;"),
      tags$p(
        style = "color: #007BFF; font-size: 14px;",
        paste0("分辨率: ", img_info$width, "x", img_info$height,
               ", 文件大小: ", round(img_info$filesize / 1024, 2), " KB")
      ),
      actionButton(ns("clear_image_preview"), "清除图片", icon = icon("trash"), class = "btn-danger", style = "margin-top: 10px;")
    )
  })
}

render_paste_prompt <- function() {
  renderUI({
    div("将图片粘贴到这里（Ctrl+V 或 Cmd+V）",
        style = "color: #888; font-size: 16px; font-style: italic;")
  })
}

# 保存图片（文件上传或粘贴）
process_image_upload <- function(sku, file_data = NULL, pasted_data = NULL, inventory_path = NULL, output_dir = "/var/www/images") {
  if (is.null(file_data) && is.null(pasted_data)) {
    # 没有上传图片，返回库存路径或 NULL
    if (!is.null(inventory_path)) {
      showNotification("使用库存中现有图片路径。", type = "message")
      return(inventory_path)
    } else {
      showNotification("未上传图片，且库存中没有对应图片路径。", type = "warning")
      return(NA)
    }
  }
  
  # 生成唯一文件名
  unique_image_name <- paste0(sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
  final_image_path <- file.path(output_dir, unique_image_name)
  
  tryCatch({
    if (!is.null(file_data)) {
      compressed_path <- save_compressed_image(
        file_path = file_data$datapath,
        output_dir = output_dir,
        image_name = unique_image_name
      )
    } else if (!is.null(pasted_data)) {
      compressed_path <- save_compressed_image(
        file_path = pasted_data$datapath,
        output_dir = output_dir,
        image_name = unique_image_name
      )
    }
    
    if (!is.null(compressed_path)) {
      showNotification("图片已成功压缩并存入数据库！", type = "message")
      return(compressed_path)
    } else {
      showNotification("图片压缩存储处理失败！", type = "error")
      return(NA)
    }
  }, error = function(e) {
    showNotification("图片上传时发生错误！", type = "error")
    return(NA)
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
                                host_url = host_url, 
                                placeholder = placeholder_50px_path) {
  
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
                                     selection = "single",
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
    selection = selection,
    filter = "top",
    rownames = FALSE,
    options = options
  )
}


update_status <- function(con, unique_id, new_status, defect_status = NULL, refresh_trigger = NULL) {
  if (!new_status %in% names(status_columns)) {
    showNotification("Invalid status provided", type = "error")
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


fetchSkuData <- function(sku, con) {
  query <- "
    SELECT 
      ItemName,
      Maker,
      MajorType,
      MinorType,
      Quantity AS TotalQuantity,
      ProductCost AS AverageCost,
      ShippingCost AS AverageShippingCost,
      ItemImagePath
    FROM inventory
    WHERE SKU = ?"
  dbGetQuery(con, query, params = list(sku))
}


fetchInventoryStatusData <- function(sku, con) {
  query <- "
    SELECT 
      Status, 
      COUNT(*) AS Count
    FROM unique_items
    WHERE SKU = ?
    GROUP BY Status"
  dbGetQuery(con, query, params = list(sku))
}


fetchDefectStatusData <- function(sku, con) {
  query <- "
    SELECT 
      Defect, 
      COUNT(*) AS Count
    FROM unique_items
    WHERE SKU = ?
    GROUP BY Defect"
  dbGetQuery(con, query, params = list(sku))
}


fetchSkuOperationData <- function(sku, con) {
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


plotBarChart <- function(data, x, y, x_label, y_label, colors) {
  # 检查数据是否为空
  if (nrow(data) == 0 || is.null(data[[y]]) || length(data[[y]]) == 0) {
    return(plotly::plot_ly(type = "scatter", mode = "text") %>%
             plotly::add_text(x = 0.5, y = 0.5, text = "无库存状态数据", textfont = list(size = 20, color = "red")))
  }
  
  # 使用 plotly 绘制柱状图
  plotly::plot_ly(
    data = data,
    x = ~get(x),
    y = ~get(y),
    type = "bar",
    marker = list(color = colors[seq_along(data[[y]])]) # 设置颜色
  ) %>%
    plotly::layout(
      xaxis = list(title = x_label),
      yaxis = list(title = y_label),
      title = "状态分布",
      showlegend = FALSE
    )
}


plotPieChart <- function(data, labels, values, colors) {
  # 检查数据是否为空
  if (nrow(data) == 0 || is.null(data[[values]]) || length(data[[values]]) == 0) {
    return(plotly::plot_ly(type = "scatter", mode = "text") %>%
             plotly::add_text(x = 0.5, y = 0.5, text = "无瑕疵情况数据", textfont = list(size = 20, color = "red")))
  }
  
  # 使用 plotly 绘制饼图
  plotly::plot_ly(
    data = data,
    labels = ~get(labels),
    values = ~get(values),
    type = "pie",
    textinfo = "label+percent",
    marker = list(colors = colors[seq_along(data[[values]])]) # 设置颜色
  ) %>%
    plotly::layout(
      title = "瑕疵情况分布",
      showlegend = TRUE
    )
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

handleSkuInput <- function(
    sku_input,        # SKU 输入值
    output_name,      # 输出 UI 名称
    count_label,      # 显示的计数标签
    count_field,      # 数据字段名称
    con,              # 数据库连接
    output,           # 输出对象
    placeholder_path, # 默认占位图片路径
    host_url          # 图片主机 URL
) {
  sku <- trimws(sku_input) # 清理空格
  
  if (is.null(sku) || sku == "") {
    # 如果 SKU 为空，渲染默认空的商品信息
    renderItemInfo(output, output_name, NULL, placeholder_path, count_label, count_field)
    return()
  }
  
  tryCatch({
    # 查询 SKU 数据
    item_info <- fetchSkuOperationData(sku, con)
    
    # 如果未找到记录
    if (nrow(item_info) == 0) {
      showNotification("未找到该条形码对应的物品！", type = "error")
      renderItemInfo(output, output_name, NULL, placeholder_path, count_label, count_field)
      return()
    }
    
    # 渲染商品信息
    renderItemInfo(
      output = output,
      output_name = output_name,
      item_info = item_info,
      img_path = ifelse(
        is.na(item_info$ItemImagePath[1]),
        placeholder_path,
        paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))
      ),
      count_label = count_label,
      count_field = count_field
    )
  }, error = function(e) {
    # 错误处理
    showNotification(paste("处理 SKU 输入时发生错误：", e$message), type = "error")
  })
}

handleOperation <- function(
    operation_name,       # 操作名称（入库、出库、售出）
    sku_input,            # SKU 输入字段
    output_name,          # 输出的 UI 名称
    query_status,         # 查询的初始状态
    update_status_value,  # 更新后的状态
    count_label,          # 显示的计数标签
    count_field,          # 计数字段名称
    con,                  # 数据库连接
    output,               # 输出对象
    refresh_trigger,      # 数据刷新触发器
    session,              # 当前会话对象
    input = NULL          # 显式传递的 input 对象
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
      ORDER BY ProductCost ASC
      LIMIT 1"), 
                            params = list(sku))
    
    if (nrow(sku_items) == 0) {
      showNotification(paste0("无可", operation_name, "的物品，所有该商品已完成 ", operation_name, "！"), type = "message")
      return()
    }
    
    # 动态设置瑕疵状态
    defect_status <- NULL
    if (operation_name == "入库" && !is.null(input)) {
      is_defective <- input$defective_item
      defect_status <- ifelse(is.null(is_defective) || !is_defective, "无瑕", "瑕疵")
    }
    
    # 更新状态
    update_status(
      con = con,
      unique_id = sku_items$UniqueID[1],
      new_status = update_status_value,
      defect_status = defect_status, # 动态传递瑕疵状态
      refresh_trigger = refresh_trigger
    )
    
    # 成功提示
    showNotification(paste0("物品成功", operation_name, "！"), type = "message")
    
    # 查询 SKU 数据并刷新 UI
    item_info <- fetchSkuOperationData(sku, con)
    
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
      updateCheckboxInput(session, "defective_item", value = FALSE) # 重置瑕疵复选框
    }
    
  }, error = function(e) {
    # 错误处理
    showNotification(paste0(operation_name, "失败：", e$message), type = "error")
  })
}

# 清理未被记录的图片 (每天运行一次)
clean_untracked_images <- function() {
  # 数据库连接信息
  con <- db_connection()
  
  tryCatch({
    # 1. 获取数据库中记录的图片路径
    query <- "SELECT ItemImagePath FROM inventory WHERE ItemImagePath IS NOT NULL"
    recorded_paths <- normalizePath(dbGetQuery(con, query)$ItemImagePath, mustWork = FALSE)
    
    # 2. 列出目录中所有图片文件，并规范化路径
    all_files <- normalizePath(list.files("/var/www/images/", full.names = TRUE), mustWork = FALSE)
    
    # 3. 检查哪些文件未被记录
    untracked_files <- setdiff(all_files, recorded_paths)
    
    # 4. 删除未被记录的文件
    if (length(untracked_files) > 0) {
      sapply(untracked_files, file.remove)
      message("以下文件已被删除：")
      print(untracked_files)
    } else {
      message("没有未被记录的文件需要清理。")
    }
  }, error = function(e) {
    message("清理过程中出现错误：", e$message)
  })
  
  # 断开数据库连接
  dbDisconnect(con)
}

updateFilters <- function(session, unique_items_data, input) {
  req(unique_items_data())
  
  validate(
    need("Maker" %in% names(unique_items_data()), "数据中缺少 Maker 列"),
    need("Status" %in% names(unique_items_data()), "数据中缺少 Status 列"),
    need("Defect" %in% names(unique_items_data()), "数据中缺少 Defect 列"),
    need("MajorType" %in% names(unique_items_data()), "数据中缺少 MajorType 列"),
    need("MinorType" %in% names(unique_items_data()), "数据中缺少 MinorType 列")
  )
  
  # 更新 Maker
  updateSelectInput(
    session,
    "maker",
    choices = unique(unique_items_data()$Maker),
    selected = if (is.null(input$maker)) unique(unique_items_data()$Maker) else input$maker
  )
  
  # 筛选数据根据 Maker
  filtered_data_maker <- unique_items_data()
  if (!is.null(input$maker)) {
    filtered_data_maker <- filtered_data_maker[filtered_data_maker$Maker %in% input$maker, ]
  }
  
  # 更新 MajorType，依赖 Maker
  updateSelectInput(
    session,
    "major_type",
    choices = unique(filtered_data_maker$MajorType),
    selected = if (is.null(input$major_type)) unique(filtered_data_maker$MajorType) else input$major_type
  )
  
  # 筛选数据根据 MajorType
  filtered_data_major <- filtered_data_maker
  if (!is.null(input$major_type)) {
    filtered_data_major <- filtered_data_major[filtered_data_major$MajorType %in% input$major_type, ]
  }
  
  # 更新 MinorType，依赖 Maker 和 MajorType
  updateSelectInput(
    session,
    "minor_type",
    choices = unique(filtered_data_major$MinorType),
    selected = if (is.null(input$minor_type)) unique(filtered_data_major$MinorType) else input$minor_type
  )
  
  # 更新 Status 和 Defect（独立于 Maker/MajorType/MinorType）
  updateSelectInput(
    session,
    "unique_status",
    choices = unique(unique_items_data()$Status),
    selected = unique(unique_items_data()$Status)
  )
  
  updateSelectInput(
    session,
    "unique_defect",
    choices = unique(unique_items_data()$Defect),
    selected = unique(unique_items_data()$Defect)
  )
  
  # 更新采购时间范围
  purchase_time <- unique_items_data()$PurchaseTime
  updateDateRangeInput(
    session,
    "purchase_time_range",
    start = min(purchase_time, na.rm = TRUE),
    end = max(purchase_time, na.rm = TRUE)
  )
}

