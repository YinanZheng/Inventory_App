
library(googledrive)

drive_auth(cache = ".secrets", email = "goldenbeanllc.bhs@gmail.com")

# 定义函数，下载 Google Drive 指定文件夹中的所有新图片
download_images_from_folder <- function(folder_name) {
  folder <- drive_get(folder_name)
  image_files <- drive_ls(folder)
  for (i in seq_len(nrow(image_files))) {
    img_file <- image_files[i, ]
    local_img_path <- file.path("./image_cache", img_file$name)
    
    # 检查本地是否已有该文件，如果没有则下载
    if (!file.exists(local_img_path)) {
      tryCatch({
        drive_download(img_file, path = local_img_path, overwrite = FALSE)
        message(paste("下载成功:", img_file$name))
      }, error = function(e) {
        message(paste("下载失败:", img_file$name, "错误:", e))
      })
    }
  }
}
