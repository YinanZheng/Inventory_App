library(googledrive)

# Authenticate with Google Drive
drive_auth(cache = ".secrets", email = "goldenbeanllc.bhs@gmail.com")

# Function to create the directory if it doesn't exist
create_image_cache_dir <- function() {
  cache_dir <- "./www/image_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Directory 'www/image_cache' created.")
  } else {
    message("Directory 'www/image_cache' already exists.")
  }
}

# Function to download Google Drive folder images to the cache directory
download_images_from_folder <- function(folder_name) {
  # Ensure the directory exists
  create_image_cache_dir()
  
  # Get the folder from Google Drive
  folder <- drive_get(folder_name)
  
  # List all files in the folder
  image_files <- drive_ls(folder)
  
  for (i in seq_len(nrow(image_files))) {
    img_file <- image_files[i, ]
    # Use the file's ID as the new filename
    local_img_path <- file.path("./www/image_cache", paste0(img_file$id, ".jpg"))
    
    # Check if the file already exists locally
    if (!file.exists(local_img_path)) {
      tryCatch({
        drive_download(img_file, path = local_img_path, overwrite = FALSE)
        message(paste("Downloaded:", img_file$name))
      }, error = function(e) {
        message(paste("Failed to download:", img_file$name, "Error:", e))
      })
    } else {
      message(paste("File already exists locally:", img_file$name))
    }
  }
}

# Execute the download function
download_images_from_folder("image")
