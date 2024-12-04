# data_loading.R
library(googlesheets4)

# Load data from Google Sheets
load_sheet_data <- function(sheet_id) {
  read_sheet(sheet_id)
}