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
setup_fonts <- function() {
  font_add("BarcodeFont", "./fonts/BarcodeFont.ttf")
  showtext_auto()
}