library(shiny)
library(readr)
library(DT)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(base64enc)

options(gargle_oauth_cache = ".secrets")

# Authenticate Google Sheets and Google Drive with pre-authorized account
gs4_auth(cache = ".secrets", email = "goldenbeanllc.bhs@gmail.com")
drive_auth(cache = ".secrets", email = "goldenbeanllc.bhs@gmail.com")

# Google Sheets and Google Drive setup
inventory_sheet_id <- "1RXcv-nPBEC-TJ9n2_Zp4fzjr-J1b_BDp75i8vxku4HM"
maker_sheet_id <- "1XNa2SEfR7c_trpWdFx8XOILzqSvkt-laSbRdnozzfRc"
major_type_sheet_id <- "1-lVcQjIgHA0tm94lFMWvinAjj2CYKs81hhVUuQRLh98"
minor_type_sheet_id <- "19FpdzrL_pSJSEolROHDFDy2OAYCEvSUIhVvFBqw0O94"
images_folder_id <- "1cjZEgGRl7BAMPUmL03gdWAe17d2sEEPb"

# Generic function to load data from Google Sheets
load_sheet_data <- function(sheet_id) {
  read_sheet(sheet_id)
}

# Save image to Google Drive
save_image_to_drive <- function(file_path, folder_id, image_name) {
  drive_file <- drive_upload(file_path, path = as_id(folder_id), name = image_name)
  drive_share(as_id(drive_file$id), role = "reader", type = "anyone")
  return(drive_file)
}

# Convert image URL to base64
convert_image_url_to_base64 <- function(file_id) {
  temp_file <- tempfile(fileext = ".png")
  drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
  base64enc::dataURI(file = temp_file, mime = "image/png")
}

# Add notification for user feedback
add_notification <- function(message, type = "message") {
  showNotification(message, type = type)
}

# Define UI
ui <- fluidPage(
  titlePanel("Inventory Management App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("barcode_search", "Search by Barcode:"),
      selectInput("new_maker", "Maker:", choices = NULL),
      selectInput("new_major_type", "Major Type:", choices = NULL),
      selectInput("new_minor_type", "Minor Type:", choices = NULL),
      hr(),
      h3("Add New Item"),
      textInput("new_Barcode", "Barcode:"),
      textInput("new_name", "Name:"),
      numericInput("new_quantity", "Quantity:", 1, min = 1),
      numericInput("new_cost", "Cost:", value = 0, min = 0, step = 0.01),
      fileInput("new_image", "Upload Image:"),
      actionButton("add_btn", "Add Item")
    ),
    
    mainPanel(
      uiOutput("item_details"),
      uiOutput("item_image")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load maker, major type, and minor type data
  maker_list <- reactiveVal(load_sheet_data(maker_sheet_id)$Maker)
  major_type_list <- reactiveVal(load_sheet_data(major_type_sheet_id)$Type)
  minor_type_list <- reactiveVal(load_sheet_data(minor_type_sheet_id)$Type)
  
  # Update maker, major type, and minor type select input choices
  observe({
    updateSelectInput(session, "new_maker", choices = maker_list())
    updateSelectInput(session, "new_major_type", choices = major_type_list())
    updateSelectInput(session, "new_minor_type", choices = minor_type_list())
  })
  
  # Load inventory data and refresh every 5 minutes
  inventory <- reactiveVal(load_sheet_data(inventory_sheet_id))
  observe({
    invalidateLater(5 * 60 * 1000) # 每5分钟重新加载一次
    inventory(load_sheet_data(inventory_sheet_id))
  })
  
  # Cache for storing image base64 data
  image_cache <- reactiveValues()
  
  # Function to render item details
  render_item_details <- function(item) {
    tagList(
      h4("Item Details:"),
      p(strong("Maker: "), item$Maker),
      p(strong("Major Type: "), item$MajorType),
      p(strong("Minor Type: "), item$MinorType),
      p(strong("Barcode: "), item$Barcode),
      p(strong("Name: "), item$ItemName),
      p(strong("Quantity: "), item$Quantity),
      p(strong("Cost: "), paste0("$", item$Cost))
    )
  }
  
  # Function to render item image
  render_item_image <- function(image_id) {
    if (!is.na(image_id)) {
      if (!is.null(image_cache[[image_id]])) {
        tags$img(src = image_cache[[image_id]], width = "200px")
      } else {
        tryCatch({
          temp_file <- tempfile(fileext = ".png")
          drive_download(as_id(image_id), path = temp_file, overwrite = TRUE)
          image_base64 <- base64enc::dataURI(file = temp_file, mime = "image/png")
          image_cache[[image_id]] <- image_base64
          tags$img(src = image_base64, width = "200px")
        }, error = function(e) {
          NULL
        })
      }
    } else {
      NULL
    }
  }
  
  # Handle search input
  observeEvent(input$barcode_search, {
    req(input$barcode_search)
    
    tryCatch({
      search_result <- inventory() %>%
        filter(Barcode == input$barcode_search)
      
      output$item_details <- renderUI({
        if (nrow(search_result) > 0) {
          render_item_details(search_result[1, ])
        } else {
          h4("No matching item found")
        }
      })
      
      output$item_image <- renderUI({
        if (nrow(search_result) == 1 && !is.na(search_result$Image[1])) {
          render_item_image(search_result$Image[1])
        } else {
          NULL
        }
      })
    }, error = function(e) {
      add_notification(paste("Error during search:", e$message), type = "error")
    })
  })
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    req(input$new_Barcode, input$new_name, input$new_quantity, input$new_cost)
    
    if (input$new_quantity <= 0 || input$new_cost < 0) {
      add_notification("Quantity and cost must be positive numbers.", type = "error")
      return()
    }
    
    # Save the uploaded image to Google Drive
    image_id <- NA
    if (!is.null(input$new_image)) {
      image_name <- paste0(input$new_Barcode, "_", input$new_image$name)
      tryCatch({
        drive_file <- save_image_to_drive(input$new_image$datapath, images_folder_id, image_name)
        image_id <- drive_file$id
      }, error = function(e) {
        add_notification(paste("Error uploading image:", e$message), type = "error")
      })
    }
    
    # Add the new item to Google Sheets
    new_item <- data.frame(
      Barcode = input$new_Barcode,
      MajorType = input$new_major_type,
      MinorType = input$new_minor_type,
      ItemName = input$new_name,
      Quantity = input$new_quantity,
      Cost = input$new_cost,
      Image = image_id,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      sheet_append(ss = inventory_sheet_id, data = new_item, sheet = "Sheet1")
      add_notification("Item added successfully.")
      inventory(load_sheet_data(inventory_sheet_id))
      
      # Show added item details
      output$item_details <- renderUI({
        render_item_details(new_item)
      })
      
      output$item_image <- renderUI({
        render_item_image(new_item$Image)
      })
    }, error = function(e) {
      add_notification(paste("Error adding item:", e$message), type = "error")
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
