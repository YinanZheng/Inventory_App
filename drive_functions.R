# drive_functions.R
library(googledrive)
library(base64enc)

# Save image to Google Drive
save_image_to_drive <- function(file_path, folder_id, image_name) {
  drive_file <- drive_upload(file_path, path = as_id(folder_id), name = image_name)
  drive_share(as_id(drive_file$id), role = "reader", type = "anyone")
  return(drive_file)
}

# Convert image URL to base64
convert_image_url_to_base64 <- function(file_id) {
  temp_file <- tempfile(fileext = ".png")
  drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
  base64enc::dataURI(file = temp_file, mime = "image/png")
}