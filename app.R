library(shiny)
library(readr)
library(DT)
library(googlesheets4)
library(googledrive)
library(dplyr)  # Load dplyr to use filter function
library(base64enc)  # Load base64enc to use base64 encoding

options(gargle_oauth_cache = ".secrets")

# Authenticate Google Sheets and Google Drive with pre-authorized account
gs4_auth(cache = ".secrets", email = "goldenbeanllc.bhs@gmail.com")
drive_auth(cache = ".secrets", email = "goldenbeanllc.bhs@gmail.com")

# Google Sheets and Google Drive setup
inventory_sheet_id <- "1RXcv-nPBEC-TJ9n2_Zp4fzjr-J1b_BDp75i8vxku4HM"  # Replace with your actual Google Sheets ID
images_folder_id <- "1cjZEgGRl7BAMPUmL03gdWAe17d2sEEPb"  # Replace with your actual Google Drive folder ID

# Load inventory data from Google Sheets
load_inventory <- function(sheet_id) {
  read_sheet(sheet_id)
}

# Save image to Google Drive
save_image_to_drive <- function(file_path, folder_id, image_name) {
  drive_file <- drive_upload(file_path, path = as_id(folder_id), name = image_name)
  
  # Set file to be shared (accessible to anyone with the link)
  drive_share(as_id(drive_file$id), role = "reader", type = "anyone")
  
  return(drive_file)
}

# Convert image URL to base64
convert_image_url_to_base64 <- function(file_id) {
  temp_file <- tempfile(fileext = ".png")
  drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
  base64enc::dataURI(file = temp_file, mime = "image/png")
}

# Define UI
ui <- fluidPage(
  titlePanel("Inventory Management App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("barcode_search", "Search by Barcode:"),
      hr(),
      
      h3("Add New Item"),
      textInput("new_Barcode", "Barcode:"),
      textInput("new_name", "Name:"),
      numericInput("new_quantity", "Quantity:", 1, min = 1),
      numericInput("new_price", "Price:", value = 0, min = 0, step = 0.01),
      fileInput("new_image", "Upload Image:"),
      actionButton("add_btn", "Add Item")
    ),
    
    mainPanel(
      DTOutput("inventory_table"),
      uiOutput("item_image")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load inventory data
  inventory <- reactiveVal(load_inventory(inventory_sheet_id))
  
  # Handle search button click
  observeEvent(input$barcode_search, {
    req(input$barcode_search)
    
    # Query Google Sheets directly without downloading
    search_result <- load_inventory(inventory_sheet_id) %>%
      filter(Barcode == input$barcode_search)
    
    output$inventory_table <- renderDT({
      datatable(search_result)
    })
    
    if (nrow(search_result) == 1 && !is.na(search_result$Image)) {
      tryCatch({
        image_base64 <- convert_image_url_to_base64(search_result$Image[1])
        output$item_image <- renderUI({
          tags$img(src = image_base64, width = "200px")
        })
      }, error = function(e) {
        output$item_image <- renderUI(NULL)
      })
    } else {
      output$item_image <- renderUI(NULL)
    }
  })
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    req(input$new_Barcode, input$new_name, input$new_quantity, input$new_price)
    
    # Save the uploaded image to Google Drive
    image_id <- NA
    if (!is.null(input$new_image)) {
      image_name <- paste0(input$new_Barcode, "_", input$new_image$name)
      drive_file <- save_image_to_drive(input$new_image$datapath, images_folder_id, image_name)
      image_id <- drive_file$id
    }
    
    # Add the new item directly to Google Sheets
    new_item <- data.frame(
      Barcode = input$new_Barcode,
      ItemName = input$new_name,
      Quantity = input$new_quantity,
      Price = input$new_price,
      Image = image_id,
      stringsAsFactors = FALSE
    )
    sheet_append(ss = inventory_sheet_id, data = new_item, sheet = "Sheet1")
    
    # Reload inventory data
    inventory(load_inventory(inventory_sheet_id))
    
    # Show updated inventory
    output$inventory_table <- renderDT({
      datatable(inventory())
    })
    
    output$item_image <- renderUI(NULL)
  })
}

# Run the Shiny app
shinyApp(ui, server)
