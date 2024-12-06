# global_setup.R
library(googlesheets4)
library(googledrive)
library(showtext)

# Google authentication
setup_google_auth <- function(email) {
  options(gargle_oauth_cache = ".secrets")
  gs4_auth(cache = ".secrets", email = email)
  drive_auth(cache = ".secrets", email = email)
}

# Font setup
setup_fonts <- function(font_names, font_dir) {
  if (length(font_names) == 0) {
    stop("字体名称列表不能为空")
  }
  
  if (!dir.exists(font_dir)) {
    stop(paste("字体目录不存在:", font_dir))
  }
  
  for (font_name in font_names) {
    font_path <- file.path(font_dir, paste0(font_name, ".ttf"))
    if (!file.exists(font_path)) {
      stop(paste("字体文件不存在:", font_path))
    }
    font_add(font_name, font_path)
  }
  
  showtext_auto()
}
