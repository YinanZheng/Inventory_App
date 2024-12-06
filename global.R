# global.R

# Load required libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(DT)

# Source all modular functions
source("config.R")
source("global_setup.R")
source("data_loading.R")
source("drive_functions.R")
source("sku_functions.R")
source("barcode_functions.R")
source("notifications.R")
source("utils.R")

# Google Auth setup
setup_google_auth("goldenbeanllc.bhs@gmail.com")

# Font setup
setup_fonts(c("code128", "consolas"), "./fonts/")