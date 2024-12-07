# Google authentication
setup_google_auth <- function(email) {
  options(gargle_oauth_cache = ".secrets")
  gs4_auth(cache = ".secrets", email = email)
  drive_auth(cache = ".secrets", email = email)
}

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

# Save compressed image to Google Drive
save_image_to_drive <- function(file_path, folder_id, image_name, quality = 75) {
  # Load the image
  img <- image_read(file_path)
  
  # Compress the image (adjusting quality and size)
  compressed_img <- image_scale(img, "800x")  # Resize image to have a width of 800px, adjust as needed
  compressed_img <- image_convert(compressed_img, format = "jpeg")
  compressed_img <- image_write(compressed_img, tempfile(fileext = ".jpg"), quality = quality)
  
  # Upload the compressed image to Google Drive
  drive_file <- drive_upload(compressed_img, path = as_id(folder_id), name = image_name)
  drive_share(as_id(drive_file$id), role = "reader", type = "anyone")
  local_img_path <- file.path("./www/image_cache", paste0(drive_file$id, ".jpg"))
  drive_download(drive_file, path = local_img_path, overwrite = FALSE)
  return(drive_file)
}

# Generate unique random letters or random digits based on the item name
generate_unique_code <- function(item_name, output_type = "letters", num_characters = 2) {
  if (!is.null(item_name) && item_name != "") {
    # Generate a hash value for item_name
    hash_value <- digest(item_name, algo = "sha256")
    
    # Split the hash value into parts and convert to a long integer seed
    hash_parts <- strsplit(hash_value, "")[[1]]
    
    # Define the length of segments to process at a time
    segment_length <- 8
    total_sum <- 0
    
    # Iterate over the hash in segments to create a combined seed value
    for (i in seq(1, length(hash_parts), by = segment_length)) {
      segment <- paste0(hash_parts[i:min(i + segment_length - 1, length(hash_parts))], collapse = "")
      segment_value <- strtoi(segment, base = 16)
      
      if (!is.na(segment_value)) {
        total_sum <- (total_sum + segment_value) %% .Machine$integer.max
      }
    }
    
    # Ensure the seed is a positive integer
    hash_seed <- abs(total_sum)
    if (hash_seed == 0) {
      hash_seed <- 1  # Ensure the seed is at least 1
    }
    
    # Set seed to ensure uniqueness based on item_name
    set.seed(hash_seed)
    
    # Generate random output based on the specified type
    if (output_type == "letters") {
      # Generate random letters
      random_output <- paste0(sample(LETTERS, num_characters, replace = TRUE), collapse = "")
    } else if (output_type == "digits") {
      # Generate random digits
      random_output <- paste0(sample(0:9, num_characters, replace = TRUE), collapse = "")
    } else {
      stop("Invalid output_type. Please use 'letters' or 'digits'.")
    }
    
    return(random_output)
  } else {
    return("")
  }
}

# Generate SKU
generate_sku <- function(item_type_data, major_type, minor_type, item_name, cost) {
  if (is.null(major_type) || is.null(minor_type) || is.null(item_name) || is.null(cost)) {
    return("")
  }
  
  # Format cost as three-digit value
  formatted_cost <- sprintf("%03.0f", round(cost))
  
  # Get major and minor type SKU values
  major_type_sku <- item_type_data %>%
    filter(MajorType == major_type) %>%
    pull(MajorTypeSKU) %>%
    unique()
  minor_type_sku <- item_type_data %>%
    filter(MinorType == minor_type) %>%
    pull(MinorTypeSKU) %>%
    unique()
  
  # Create the SKU
  paste0(major_type_sku, formatted_cost, minor_type_sku, 
         generate_unique_code(item_name, "digits", 1),
         generate_unique_code(item_name, "letters", 2))
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
                                local_image_dir = "image_cache", 
                                placeholder = "https://dummyimage.com/50x50/cccccc/000000.png&text=No+Image") {
  sapply(image_column, function(img) {
    if (is.na(img) || img == "") {
      # 占位图片逻辑
      return(paste0('<img src="', placeholder, '" width="50" height="50" style="object-fit:cover;"/>'))
    }
    
    if (is_local) {
      # 本地图片逻辑
      local_img_path <- file.path(local_image_dir, paste0(img, ".jpg"))
      # print(local_img_path)
      if (file.exists(file.path("./www", local_img_path))) {
        return(paste0('<img src="', local_img_path, '" width="50" height="50" style="object-fit:cover;"/>'))
      } else {
        # 本地图片不存在时返回占位图片
        return(paste0('<img src="', placeholder, '" width="50" height="50" style="object-fit:cover;"/>'))
      }
    } else {
      # 远程 URL 或 Base64 图片逻辑
      return(paste0('<img src="', img, '" width="50" height="50" style="object-fit:cover;"/>'))
    }
  })
}

# 通用函数：渲染表格（支持图片列处理和列名映射）
render_table_with_images <- function(data, 
                                     column_mapping, 
                                     image_column = NULL, 
                                     is_local = FALSE, 
                                     local_image_dir = "image_cache", 
                                     placeholder = "https://dummyimage.com/50x50/cccccc/000000.png&text=No+Image") {
  if (!is.null(image_column) && nrow(data) > 0) {
    # 渲染图片列
    data[[image_column]] <- render_image_column(
      data[[image_column]],
      is_local = is_local,
      local_image_dir = local_image_dir,
      placeholder = placeholder
    )
  }
  
  # 映射列名
  data <- map_column_names(data, column_mapping)
  
  # 返回渲染后的 datatable 对象
  datatable(
    data,
    escape = FALSE,  # 禁用 HTML 转义
    selection = 'single'
  )
}