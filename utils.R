# Map column names and filter only mapped columns
map_column_names <- function(data, column_mapping) {
  # 获取 column_mapping 中的列名
  mapped_columns <- intersect(names(column_mapping), names(data))
  
  # 如果没有匹配的列，返回空表
  if (length(mapped_columns) == 0) {
    return(data.frame())
  }
  
  # 筛选并重命名列
  data <- data[, mapped_columns, drop = FALSE]  # 只保留映射中提到的列
  setNames(data, column_mapping[mapped_columns])  # 更新列名
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
  
  pdf_path <- paste0(temp_dir, "/", sku, "_barcode")  # 组合文件夹路径和文件名
  
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

# Upload compressed image to server
save_compressed_image <- function(file_path, output_dir = "/var/www/images", image_name = NULL, quality = 75) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate image name if not provided
  if (is.null(image_name)) {
    image_name <- paste0(tools::file_path_sans_ext(basename(file_path)), "_compressed.jpg")
  }
  
  # Load the image
  img <- magick::image_read(file_path)
  
  # Compress the image (resize and adjust quality)
  compressed_img <- magick::image_scale(img, "800x")  # Resize image to width of 800px
  compressed_img <- magick::image_convert(compressed_img, format = "jpeg")
  compressed_img_path <- file.path(output_dir, image_name)
  magick::image_write(compressed_img, path = compressed_img_path, quality = quality)
  
  # Return the path to the compressed image
  return(compressed_img_path)
}

# Generate unique code
generate_unique_code <- function(item_name, maker, length = 4) {
  if (!is.null(item_name) && item_name != "" && !is.null(maker) && maker != "") {
    # Combine item_name and maker to generate a unique hash
    combined_input <- paste(item_name, maker, sep = "_")
    hash_value <- digest(combined_input, algo = "sha256")
    
    # Convert hash to numeric seed
    hash_numeric <- as.numeric(strtoi(substr(hash_value, 1, 8), base = 16))
    set.seed(abs(hash_numeric) %% .Machine$integer.max)
    
    # Generate a random alphanumeric code
    random_output <- paste0(sample(c(LETTERS, 0:9), length, replace = TRUE), collapse = "")
    return(random_output)
  }
  return("")
}

generate_sku <- function(item_type_data, major_type, minor_type, item_name, maker) {
  if (is.null(major_type) || is.null(minor_type) || is.null(item_name) || is.null(maker)) {
    return("")
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
    return("")  # Return empty if no matching SKUs found
  }
  
  # Generate unique code using Maker and ItemName
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

# 定义空表模板函数
create_empty_inventory <- function() {
  data.frame(
    SKU = character(),
    Maker = character(),
    MajorType = character(),
    MinorType = character(),
    ItemName = character(),
    Quantity = integer(),
    Cost = integer(),
    ItemImage = character(),
    ItemImagePath = character(),
    stringsAsFactors = FALSE
  )
}

# 通用函数：渲染图片列（支持本地图片路径和 URL/Base64 图片）
render_image_column <- function(image_column, 
                                is_local = FALSE, 
                                local_image_dir = "www/images", 
                                placeholder = "https://dummyimage.com/50x50/cccccc/000000.png&text=No+Image") {
  sapply(image_column, function(img) {
    if (is.na(img) || img == "") {
      # Return placeholder image for missing data
      return(paste0('<img src="', placeholder, '" width="50" height="50" style="object-fit:cover;"/>'))
    }
    
    if (is_local) {
      # Handle local images
      local_img_path <- file.path(local_image_dir, basename(img))
      if (file.exists(local_img_path)) {
        return(paste0('<img src="', local_img_path, '" width="50" height="50" style="object-fit:cover;"/>'))
      } else {
        # Return placeholder if local file does not exist
        return(paste0('<img src="', placeholder, '" width="50" height="50" style="object-fit:cover;"/>'))
      }
    } else {
      # Handle remote URL or Base64 image
      return(paste0('<img src="', img, '" width="50" height="50" style="object-fit:cover;"/>'))
    }
  }, USE.NAMES = FALSE)
}

# 通用函数：渲染表格（支持图片列处理和列名映射）
render_table_with_images <- function(data, 
                                     column_mapping, 
                                     image_column = NULL, 
                                     is_local = FALSE, 
                                     local_image_dir = "www/images", 
                                     placeholder = "https://dummyimage.com/50x50/cccccc/000000.png&text=No+Image") {
  if (!is.null(image_column) && nrow(data) > 0) {
    # Render the image column
    data[[image_column]] <- render_image_column(
      data[[image_column]],
      is_local = is_local,
      local_image_dir = local_image_dir,
      placeholder = placeholder
    )
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