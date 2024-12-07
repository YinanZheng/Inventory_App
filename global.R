# global.R

# Load required libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(DT)
library(digest) # For has value
library(magick) # For compression
library(googlesheets4)
library(googledrive)
library(stringi)
library(baRcodeR)

# Source all modular functions
source("supplier_module.R")
source("utils.R")

# Google IDs
inventory_sheet_id <- "1RXcv-nPBEC-TJ9n2_Zp4fzjr-J1b_BDp75i8vxku4HM"
maker_sheet_id <- "1XNa2SEfR7c_trpWdFx8XOILzqSvkt-laSbRdnozzfRc"
item_type_sheet_id <- "1-lVcQjIgHA0tm94lFMWvinAjj2CYKs81hhVUuQRLh98"
images_folder_id <- "1cjZEgGRl7BAMPUmL03gdWAe17d2sEEPb"

# Size of barcode paper (in cm)
page_width = 4
page_height = 2
size_unit = "cm"

# Google Auth setup
setup_google_auth("goldenbeanllc.bhs@gmail.com")