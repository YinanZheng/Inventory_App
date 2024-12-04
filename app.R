# app.R
library(shiny)
library(dplyr)

# Source all modular functions
source("global_setup.R")
source("data_loading.R")
source("drive_functions.R")
source("barcode_functions.R")

# Google Auth setup
setup_google_auth("goldenbeanllc.bhs@gmail.com")

# Font setup
setup_fonts()

# Define UI
ui <- fluidPage(
  titlePanel("库存管理系统"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sku_search", "按SKU搜索:"),
      hr(),
      h3("入库"),
      selectInput("new_maker", "织女:", choices = NULL),
      selectInput("new_major_type", "大类:", choices = NULL),
      selectInput("new_minor_type", "小类:", choices = NULL),
      textInput("new_name", "商品名:"),
      numericInput("new_quantity", "数量:", 1, min = 1),
      numericInput("new_cost", "成本:", value = 0, min = 0, max = 999, step = 1),
      textInput("new_Barcode", "SKU:", value = ""),
      fileInput("new_image", "商品图片:"),
      actionButton("add_btn", "添加商品"),
      actionButton("export_btn", "导出条形码")
    ),
    
    mainPanel(
      uiOutput("item_details"),
      uiOutput("item_image"),
      uiOutput("barcode_image")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load data
  maker_list <- reactive(load_sheet_data(maker_sheet_id)$Maker)
  item_type_data <- reactive(load_sheet_data(item_type_sheet_id))
  inventory <- reactiveVal(load_sheet_data(inventory_sheet_id))
  
  # Update maker, major type, and minor type select input choices
  observe({
    updateSelectInput(session, "new_maker", choices = maker_list())
    updateSelectInput(session, "new_major_type", choices = unique(item_type_data()$MajorType))
  })
  
  # Update minor type choices based on selected major type
  observeEvent(input$new_major_type, {
    req(input$new_major_type)
    minor_types <- item_type_data() %>%
      filter(MajorType == input$new_major_type) %>%
      pull(MinorType)
    updateSelectInput(session, "new_minor_type", choices = minor_types)
  })
  
  # Refresh inventory data every 5 minutes
  observe({
    invalidateLater(5 * 60 * 1000)
    inventory(load_sheet_data(inventory_sheet_id))
  })
  
  # Cache for storing image base64 data
  image_cache <- reactiveValues()
  
  # Function to render item details
  render_item_details <- function(item) {
    tagList(
      h4("Item Details:"),
      p(strong("织女: "), item$Maker),
      p(strong("大类: "), item$MajorType),
      p(strong("小类: "), item$MinorType),
      p(strong("库存单位: "), item$SKU),
      p(strong("商品名: "), item$ItemName),
      p(strong("库存: "), item$Quantity),
      p(strong("成本: "), paste0("$", item$Cost))
    )
  }
  
  # Function to render item image
  render_item_image <- function(image_id) {
    if (is.na(image_id)) return(NULL)
    if (!is.null(image_cache[[image_id]])) {
      return(tags$img(src = image_cache[[image_id]], width = "200px"))
    }
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
  
  # Handle search input
  observeEvent(input$sku_search, {
    req(input$sku_search)
    
    tryCatch({
      search_result <- inventory() %>% filter(SKU == input$sku_search)
      
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
      
      output$barcode_image <- renderUI({
        if (nrow(search_result) == 1) {
          tags$img(src = generate_barcode(search_result$SKU[1]), width = "200px")
        } else {
          NULL
        }
      })
    }, error = function(e) {
      show_custom_notification(paste("Error during search:", e$message), type = "error")
    })
  })
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    req(input$new_Barcode, input$new_name, input$new_quantity, input$new_cost)
    new_cost_rounded <- round(input$new_cost)
    
    if (input$new_quantity <= 0 || input$new_cost < 0 || input$new_cost > 999) {
      show_custom_notification("数量必须为正数，且成本必须在0到999之间。", type = "error")
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
        show_custom_notification(paste("上传图片时出错:", e$message), type = "error")
      })
    }
    
    # Add the new item to Google Sheets
    new_item <- data.frame(
      SKU = input$new_Barcode,
      MajorType = input$new_major_type,
      MinorType = input$new_minor_type,
      ItemName = input$new_name,
      Quantity = input$new_quantity,
      Cost = new_cost_rounded,
      Image = image_id,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      sheet_append(ss = inventory_sheet_id, data = new_item, sheet = "Sheet1")
      show_custom_notification("商品添加成功。")
      inventory(load_sheet_data(inventory_sheet_id))
      
      # Show added item details
      output$item_details <- renderUI({
        render_item_details(new_item)
      })
      
      output$item_image <- renderUI({
        render_item_image(new_item$Image)
      })
      
      output$barcode_image <- renderUI({
        tags$img(src = generate_barcode(new_item$SKU), width = "200px")
      })
    }, error = function(e) {
      show_custom_notification(paste("添加商品时出错:", e$message), type = "error")
    })
  })
  
  # Handle export barcode button click
  observeEvent(input$export_btn, {
    req(input$new_Barcode, input$new_quantity)
    pdf_file <- export_barcode_pdf(input$new_Barcode, input$new_quantity)
    show_custom_notification("条形码已导出为PDF。")
    output$barcode_pdf <- downloadHandler(
      filename = function() {
        paste0("barcode_", input$new_Barcode, ".pdf")
      },
      content = function(file) {
        file.copy(pdf_file, file)
      }
    )
  })
  
  # Automatically generate SKU when relevant inputs change
  observeEvent({input$new_cost; input$new_major_type; input$new_minor_type; input$new_name}, {
    req(input$new_major_type, input$new_minor_type, input$new_name, input$new_cost)
    
    # Format cost as three-digit value
    formatted_cost <- sprintf("%03.0f", round(input$new_cost))
    
    # Generate two random letters based on the item name
    random_letters <- if (!is.null(input$new_name) && input$new_name != "") {
      set.seed(as.integer(Sys.time()))
      paste0(sample(LETTERS, 2, replace = TRUE), collapse = "")
    } else {
      ""
    }
    
    # Get major and minor type SKU values
    major_type_sku <- item_type_data() %>%
      filter(MajorType == input$new_major_type) %>%
      pull(MajorTypeSKU) %>%
      unique()
    minor_type_sku <- item_type_data() %>%
      filter(MinorType == input$new_minor_type) %>%
      pull(MinorTypeSKU) %>%
      unique()
    
    # Create the SKU
    generated_sku <- paste0(major_type_sku, minor_type_sku, formatted_cost, random_letters)
    
    updateTextInput(session, "new_Barcode", value = as.character(generated_sku))
  })
}

# Run the Shiny app
shinyApp(ui, server)