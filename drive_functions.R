# Load necessary libraries
library(googledrive)
library(base64enc)
library(magick)

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

# Convert image URL to base64
convert_image_url_to_base64 <- function(file_id) {
  temp_file <- tempfile(fileext = ".png")
  drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
  base64enc::dataURI(file = temp_file, mime = "image/png")
}
