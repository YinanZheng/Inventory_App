# app.R
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
font_family = "BarcodeFont"
setup_fonts(font_family, "./fonts/IDAutomationHC39M Free Version.ttf")

# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("库存管理系统"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, selectizeInput("new_maker", "供应商:", choices = NULL, 
                                  options = list(placeholder = '输入供应商名称（或拼音）进行搜索', maxOptions = 500)))
      ),
      fluidRow(
        column(6, uiOutput("major_type_ui")), ## 大类
        column(6, uiOutput("minor_type_ui")), ## 小类
      ),
      fluidRow(
        column(12, textInput("new_name", "商品名:"))
      ),
      fluidRow(
        column(6, numericInput("new_quantity", "数量:", value = 1, min = 1, step = 1)),
        column(6, numericInput("new_cost", "成本:", value = 0, min = 0, max = 999, step = 1))
      ),
      textInput("new_sku", "SKU(自动生成):", value = ""),
      fileInput("new_item_image", "商品图片:"),
      
      tags$hr(), # 分隔线
      h4("入库操作"),
      actionButton("add_btn", "添加商品"),
      actionButton("confirm_btn", "确认入库", class = "btn-primary"),
      
      tags$hr(), # 分隔线
      h4("条形码操作"),
      actionButton("export_btn", "生成条形码"),
      downloadButton("barcode_pdf", "下载条形码"),
      
      tags$hr(), # 分隔线
      actionButton("reset_btn", "重置", icon = icon("undo"), class = "btn-danger")
    ),
    
    mainPanel(
      # 使用 fluidRow 分成上下两个部分
      fluidRow(
        column(12,  div(
          h4("符合条件的库存记录"), 
          style = "font-size: 20px; font-weight: bold; color: #333; background-color: #f9f9f9; 
             padding: 3px; border: 2px solid #ddd; border-radius: 3px; text-align: center;"
        )),
        column(12, DTOutput("filtered_inventory_table"))
      ),
      
      tags$hr(), # 分隔线
      fluidRow(
        column(12, div(
          h4("已添加商品"), 
          style = "font-size: 20px; font-weight: bold; color: #333; background-color: #f9f9f9; 
             padding: 3px; border: 2px solid #ddd; border-radius: 3px; text-align: center;"
        )),
        column(12, DTOutput("added_items_table")),
        column(12, actionButton("delete_btn", "删除选中记录", icon = icon("trash"))),
      ),
      
      tags$hr(), # 分隔线
      div(
        textOutput("total_cost"),
        style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;"
      )
    )
  )
)

### Define server logic
server <- function(input, output, session) {
  # Load data
  maker_list <- reactive(load_sheet_data(maker_sheet_id))
  item_type_data <- reactive(load_sheet_data(item_type_sheet_id))
  inventory <- reactiveVal(load_sheet_data(inventory_sheet_id))
  
  # Update maker select input choices
  observe({
    choices <- setNames(maker_list()$Maker, paste0(maker_list()$Maker, "(", maker_list()$Pinyin, ")"))
    updateSelectizeInput(session, "new_maker", choices = choices, server = TRUE)
  })
  
  # Render Major Type Dropdown
  output$major_type_ui <- renderUI({
    type_data <- item_type_data()
    unique_majors <- unique(type_data[, c("MajorType", "MajorTypeSKU")])
    choices <- setNames(unique_majors$MajorType, paste0(unique_majors$MajorType, "（", unique_majors$MajorTypeSKU, "）"))
    selectInput("new_major_type", "大类:", choices = choices)
  })
  
  # Render Minor Type Dropdown dynamically
  output$minor_type_ui <- renderUI({
    type_data <- item_type_data()
    
    if (!is.null(input$new_major_type)) {
      selected_major <- gsub("（.*）", "", input$new_major_type)
      # Filter rows for the selected major_type
      filtered_data <- type_data[type_data$MajorType == selected_major, ]
      choices <- setNames(filtered_data$MinorType, paste0(filtered_data$MinorType, "（", filtered_data$MinorTypeSKU, "）"))
      selectInput("new_minor_type", "小类:", choices = choices)
    }
  })
  
  # Filter inventory based on major and minor type
  filtered_inventory <- reactive({
    req(input$new_major_type, input$new_minor_type)
    if (input$new_major_type == "" || input$new_minor_type == "") {
      return(inventory() %>% select(-ItemImagePath))
    }
    inventory() %>%
      filter(MajorType == input$new_major_type, MinorType == input$new_minor_type) %>% select(-ItemImagePath)
  })
  
  # Render filtered inventory with column name mapping
  output$filtered_inventory_table <- renderDT({
    column_mapping <- list(
      SKU = "条形码",
      MajorType = "大类",
      MinorType = "小类",
      ItemName = "商品名",
      Quantity = "库存数",
      Cost = "采购成本"
    )
    datatable(
      map_column_names(filtered_inventory(), column_mapping),
      selection = 'single',
      rownames = FALSE
    )
  })
  
  # Refresh inventory data every 5 minutes
  observe({
    invalidateLater(5 * 60 * 1000)
    inventory(load_sheet_data(inventory_sheet_id))
  })
  
  # Cache for storing image base64 data
  image_cache <- reactiveValues()
  
  # Function to render item image
  render_item_image <- function(image_id) {
    if (is.na(image_id)) return(NULL)
    if (!is.null(image_cache[[image_id]])) {
      return(tags$img(src = image_cache[[image_id]], width = "200px"))
    }
    tryCatch({
      image_cache[[image_id]] <- convert_image_url_to_base64(image_id)
      tags$img(src = image_base64, width = "200px")
    }, error = function(e) {
      NULL
    })
  }
  
  # Reactive value to store added items
  added_items <- reactiveVal(data.frame(
    SKU = character(),
    Maker = character(),
    MajorType = character(),
    MinorType = character(),
    ItemName = character(),
    Quantity = integer(),
    Cost = integer(),
    ItemImage = character(),
    ItemImagePath = character(),
    stringsAsFactors = FALSE
  ))
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    req(input$new_sku, input$new_name, input$new_quantity, input$new_cost)
    
    image_data <- if (!is.null(input$new_item_image)) {
      base64enc::dataURI(file = input$new_item_image$datapath, mime = input$new_item_image$type)
    } else {
      NA
    }
    
    new_item <- data.frame(
      SKU = input$new_sku,
      Maker = input$new_maker,
      MajorType = input$new_major_type,
      MinorType = input$new_minor_type,
      ItemName = input$new_name,
      Quantity = input$new_quantity,
      Cost = round(input$new_cost),
      ItemImage = image_data,
      ItemImagePath = if (!is.null(input$new_item_image)) input$new_item_image$datapath else NA,
      stringsAsFactors = FALSE
    )
    
    # Update the added items reactive value
    added_items(bind_rows(added_items(), new_item))
  })
  
  # Render added items table
  output$added_items_table <- renderDT({
    
    items <- added_items()
    
    # 如果数据框为空，初始化空数据框
    if (nrow(items) == 0) {
      items <- data.frame(
        SKU = character(),
        Maker = character(),
        MajorType = character(),
        MinorType = character(),
        ItemName = character(),
        Quantity = integer(),
        Cost = numeric(),
        ItemImage = character(),
        stringsAsFactors = FALSE
      )
    }
    
    # 列映射
    column_mapping <- list(
      SKU = "条形码",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      ItemName = "商品名",
      Quantity = "入库数量",
      Cost = "采购成本",
      ItemImage = "商品图片"
    )
    
    # 渲染图片列为 HTML
    items$ItemImage <- sapply(items$ItemImage, function(img) {
      if (!is.na(img) && nzchar(img)) {
        paste0('<img src="', img, '" width="100" height="100"/>')
      } else {
        ""
      }
    })
    
    items <- map_column_names(items, column_mapping)
    
    if("ItemImagePath" %in% colnames(items)) items <- items %>% select(-ItemImagePath)
    
    # 渲染数据表格
    datatable(
      items,
      escape = FALSE,  # 禁用 HTML 转义
      selection = "multiple", 
      options = list(
        autoWidth = TRUE, responsive = TRUE,
        columnDefs = list(
          list(targets = which(names(items) == "ItemImage"), className = "dt-center")
        )
      )
    )
  })
  
  # Delete selected item
  observeEvent(input$delete_btn, {
    selected_row <- input$added_items_table_rows_selected
    if (length(selected_row) > 0) {
      current_items <- added_items()
      updated_items <- current_items[-selected_row, ]  # Remove selected row
      added_items(updated_items)  # Update reactive value
      showNotification("记录已成功删除", type = "message")
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
  })
  
  output$total_cost <- renderText({
    total <- sum(added_items()$Quantity * added_items()$Cost)
    paste0("本次入库总金额: ¥", format(total, big.mark = ",", scientific = FALSE))
  })
  
  
  # Flag to determine if SKU should be auto-generated
  auto_generate_sku <- reactiveVal(TRUE)
  
  # Handle row selection in filtered inventory table
  observeEvent(input$filtered_inventory_table_rows_selected, {
    selected_row <- input$filtered_inventory_table_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- filtered_inventory()[selected_row, ]
      
      # Update the input fields in the sidebar
      auto_generate_sku(FALSE)  # Disable auto generation of SKU when selecting a record
      updateSelectInput(session, "new_major_type", selected = selected_data$MajorType)
      updateSelectInput(session, "new_minor_type", selected = selected_data$MinorType)
      updateTextInput(session, "new_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = selected_data$Quantity)
      updateNumericInput(session, "new_cost", value = selected_data$Cost)
      updateTextInput(session, "new_sku", value = selected_data$SKU)
    }
  })
  
  # Handle confirm button click to update the database
  observeEvent(input$confirm_btn, {
    # If no items are added, show an error notification
    if (nrow(added_items()) == 0) {
      show_custom_notification("请先添加至少一个商品再确认!", type = "error")
      return()
    }
    
    # Upload images to Google Drive
    added_items_df <- added_items()
    for (i in 1:nrow(added_items_df)) {
      if (!is.na(added_items_df$ItemImagePath[i])) {
        tryCatch({
          drive_file <- save_image_to_drive(added_items_df$ItemImagePath[i], images_folder_id, added_items_df$SKU[i])
          #Update image path to google drive path
          added_items_df$ItemImagePath[i] <- drive_file$id
          show_custom_notification(paste("图片上传成功! SKU:", added_items_df$SKU[i], ", 商品名:", added_items_df$ItemName[i]), type = "message")
        }, error = function(e) {
          show_custom_notification(paste("图片上传失败! SKU:", added_items_df$SKU[i], ", 商品名:", added_items_df$ItemName[i]), type = "error")
        })
      }
    }
    
    # Update the Google Sheet with the added items (excluding Maker and ItemImage)
    # tryCatch({
    # Loop through added items to either update or add
    for (i in 1:nrow(added_items_df)) {
      sku <- added_items_df$SKU[i]
      
      if (sku %in% inventory()$SKU) {
        # Update existing item quantity in Google Sheets
        sheet_range <- which(inventory()$SKU == sku)
        if (length(sheet_range) == 1) {
          inventory_quantity <- ifelse(is.na(inventory()$Quantity[sheet_range]), 0, as.integer(inventory()$Quantity[sheet_range]))
          added_quantity <- ifelse(is.na(added_items_df$Quantity[i]), 0, as.integer(added_items_df$Quantity[i]))
          updated_quantity <- inventory_quantity + added_quantity
          range_write(
            ss = inventory_sheet_id, 
            data = data.frame(updated_quantity), 
            sheet = "Sheet1", 
            range = paste0("E", sheet_range + 1), 
            col_names = FALSE 
          )
          show_custom_notification(paste("库存更新成功! SKU:", sku, ", 当前库存数:", updated_quantity), type = "message")
        } else {
          show_custom_notification(paste("找到多条记录SKU:", sku), type = "error")
        }
      } else {
        # Add new item
        sheet_append(ss = inventory_sheet_id, data = added_items_df[i, ] %>% select(-Maker, -ItemImage), sheet = "Sheet1")
        show_custom_notification(paste("新商品添加成功! SKU:", sku, ", 商品名:", added_items_df$ItemName[i]), type = "message")
      }
    }
    
    inventory(load_sheet_data(inventory_sheet_id)) # Refresh the inventory to reflect any updates
    
    # }, error = function(e) {
    # show_custom_notification("更新数据库时出错!", type = "error")
    # })
    
    # Clear the added items after confirming
    added_items(data.frame(
      SKU = character(),
      Maker = character(),
      MajorType = character(),
      MinorType = character(),
      ItemName = character(),
      Quantity = integer(),
      Cost = integer(),
      ItemImage = character(),
      ItemImagePath = character(),
      stringsAsFactors = FALSE
    ))
    
    # Update the filtered DT table
    output$filtered_inventory_table <- renderDT({
      datatable(filtered_inventory(), selection = 'single', rownames = FALSE)
    })
  })
  
  # Generate Barcode based on SKU
  session$onFlushed(function() {
    shinyjs::disable("barcode_pdf")
  })
  
  pdf_file_path <- reactiveVal(NULL)
  
  observeEvent(input$export_btn, {
    req(input$new_sku, input$new_quantity)
    pdf_file <- export_barcode_pdf(input$new_sku, input$new_quantity, font_family)
    pdf_file_path(pdf_file)
    show_custom_notification("条形码已导出为PDF!")
    shinyjs::enable("barcode_pdf")
  })
  
  # Download PDF button
  output$barcode_pdf <- downloadHandler(
    filename = function() {
      paste0("barcode_", input$new_sku, ".pdf")
    },
    content = function(file) {
      file.copy(pdf_file_path(), file, overwrite = TRUE)
    }
  )
  
  # Automatically generate SKU when relevant inputs change
  observeEvent({input$new_cost; input$new_major_type; input$new_minor_type; input$new_name}, {
    req(input$new_major_type, input$new_minor_type, input$new_name, input$new_cost)
    
    # Only generate SKU if auto_generate_sku is TRUE
    if (auto_generate_sku()) {
      sku <- generate_sku(item_type_data(), 
                          input$new_major_type, 
                          input$new_minor_type, 
                          input$new_name, 
                          input$new_cost)
      updateTextInput(session, "new_sku", value = sku)
    }
  })
  
  observeEvent(input$reset_btn, {
    updateSelectizeInput(session, "new_maker", choices = maker_list()$Maker, server = TRUE)
    updateSelectInput(session, "new_major_type", selected = NULL)
    updateSelectInput(session, "new_minor_type", selected = NULL)
    updateTextInput(session, "new_name", value = "")
    updateNumericInput(session, "new_quantity", value = 1)
    updateNumericInput(session, "new_cost", value = 0)
    updateTextInput(session, "new_sku", value = "")
    shinyjs::reset("new_item_image")
    
    added_items(data.frame(
      SKU = character(),
      Maker = character(),
      MajorType = character(),
      MinorType = character(),
      ItemName = character(),
      Quantity = integer(),
      Cost = integer(),
      ItemImage = character(),
      ItemImagePath = character(),
      stringsAsFactors = FALSE
    ))
    
    inventory(load_sheet_data(inventory_sheet_id))
    
    output$filtered_inventory_table <- renderDT({
      datatable(filtered_inventory(), selection = 'single', rownames = FALSE)
    })
    
    show_custom_notification("已重置所有输入和状态！", type = "message")
  })
}

# Run the Shiny app
shinyApp(ui, server)