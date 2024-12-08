# global.R

# Load required libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(DT)
library(digest) # For has value
library(magick) # For compression
library(stringi)
library(baRcodeR)
library(DBI)
library(RMariaDB)

# Source all modular functions
source("./modules/supplier_module.R")
source("utils.R")

# Cache setup
cache_interval <- 5 * 60 * 1000  # Cache refresh interval (5 minutes)

# Size of barcode paper (in cm)
page_width = 4
page_height = 2
size_unit = "cm"

