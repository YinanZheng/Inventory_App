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
generate_unique_code <- function(item_name, maker, length = 4) {
  # Validate inputs
  if (is.null(item_name) || item_name == "" || is.null(maker) || maker == "") {
    return(paste0(rep("X", length), collapse = ""))  # Default output for invalid input
  }
    
  # Validate length parameter
  if (!is.numeric(length) || length <= 0) {
    stop("Length must be a positive integer.")
  }
  
  # Combine item_name and maker to generate a unique hash
  combined_input <- paste(item_name, maker, sep = "_")
  
  # Generate a hash value
  hash_value <- digest::digest(enc2utf8(combined_input), algo = "sha512")
  
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
  unique_code <- generate_unique_code(item_name, maker, length = 4)
  
  # Create the SKU in the format: MajorTypeSKU-MinorTypeSKU-UniqueCode
  paste0(major_type_sku, "-", minor_type_sku, "-", unique_code)
}

# Unified function for showing notifications
show_custom_notification <- function(message, type = "message") {
  showNotification(
    paste0(if (type == "error") "错误: " else "提示: ", message),
    type = type,
    duration = 10
  )
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
    Cost = numeric(),             # Cost
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
      print(img_path)  # 打印路径进行调试
      paste0('<img src="', img_path, '" width="50" height="50" style="object-fit:cover;"/>')
    }
  }, USE.NAMES = FALSE)
}

# Function to render datatable with images
render_table_with_images <- function(data, 
                                     column_mapping, 
                                     image_column = NULL) {
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
    selection = 'single'
  )
}

update_status <- function(con, unique_id, new_status) {
  # 定义状态到时间戳列的映射
  status_columns <- list(
    "国内仓入库" = "DomesticEntryTime",
    "国内仓出库" = "DomesticExitTime",
    "美国仓入库" = "UsEntryTime",
    "美国仓出库" = "UsExitTime"
  )
  
  # 检查状态是否有效
  if (!new_status %in% names(status_columns)) {
    stop("Invalid status provided")
  }
  
  # 获取对应的时间戳列
  timestamp_column <- status_columns[[new_status]]
  
  # 更新数据库中的状态和时间戳
  dbExecute(con, 
            paste0("UPDATE unique_items SET Status = ?, ", timestamp_column, " = ? WHERE UniqueID = ?"),
            params = list(new_status, Sys.time(), unique_id))
}