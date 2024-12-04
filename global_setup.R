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
setup_fonts <- function(font_name, font_path) {
  if (!file.exists(font_path)) {
    stop(paste("字体文件不存在:", font_path))
  }
  font_add(font_name, font_path)
  showtext_auto()
}