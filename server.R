# Connect to MySQL Database
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "inventory_system",
  host = "localhost",
  user = "root",
  password = "goldenbeanllc",
  encoding = "utf8mb4"
)

# Define server logic
server <- function(input, output, session) {
  # Load data from MySQL
  # maker_list <- reactive({
  #   dbGetQuery(con, "SELECT * FROM maker_list")
  # })
  
  inventory <- reactiveVal({
    dbGetQuery(con, "SELECT * FROM inventory")
  })
  
  # Reactive value to store added items
  added_items <- reactiveVal(create_empty_inventory())
  
  
  ## 供应商模块
  supplier_module(input, output, session, con)
  
  
  ## 大小类模块
  # Fetch and prepare item type data from the database
  item_type_data <- reactive({
    tryCatch({
      dbGetQuery(con, "SELECT * FROM item_type_data")
    }, error = function(e) {
      print(paste("Error fetching item_type_data:", e$message))
      NULL
    })
  })
  
  # Render Major Type Dropdown
  output$major_type_ui <- renderUI({
    req(item_type_data())  # Ensure item_type_data is not NULL
    
    # Extract unique major types with their SKUs
    type_data <- item_type_data()
    unique_majors <- unique(type_data[, c("MajorType", "MajorTypeSKU")])
    
    # Ensure there are available major types
    if (nrow(unique_majors) == 0) {
      return(h5("无可用大类"))
    }
    
    # Generate choices for the dropdown
    choices <- setNames(
      unique_majors$MajorType, 
      paste0(unique_majors$MajorType, "（", unique_majors$MajorTypeSKU, "）")
    )
    
    selectInput("new_major_type", "大类:", choices = choices, selected = NULL)
  })
  
  # Render Minor Type Dropdown dynamically
  output$minor_type_ui <- renderUI({
    req(item_type_data(), input$new_major_type)  # Ensure required inputs are available
    
    # Extract and filter for the selected major type
    type_data <- item_type_data()
    selected_major <- gsub("（.*）", "", input$new_major_type)  # Remove SKU from the display
    
    # Filter minor types for the selected major type
    filtered_data <- type_data[type_data$MajorType == selected_major, ]
    
    # Ensure there are available minor types
    if (nrow(filtered_data) == 0) {
      return(h5("无可用小类"))
    }
    
    # Generate choices for the dropdown
    choices <- setNames(
      filtered_data$MinorType, 
      paste0(filtered_data$MinorType, "（", filtered_data$MinorTypeSKU, "）")
    )
    
    selectInput("new_minor_type", "小类:", choices = choices, selected = NULL)
  })
  
  
  ## 库存表渲染模块
  
  # Filter inventory based on major and minor type (ensure Maker is included)
  filtered_inventory <- reactive({
    req(input$new_major_type, input$new_minor_type)
    
    # Filter the inventory data and include Maker
    result <- inventory() %>%
      filter(MajorType == input$new_major_type, MinorType == input$new_minor_type) %>%
      select(SKU, Maker, MajorType, MinorType, ItemName, Quantity, Cost, ItemImagePath)  # Ensure Maker is included
    
    # Return empty inventory if no results
    if (nrow(result) == 0) {
      return(create_empty_inventory())
    }
    
    return(result)
  })
  
  # Render filtered inventory with column name mapping
  output$filtered_inventory_table <- renderDT({
    column_mapping <- list(
      SKU = "条形码",
      Maker = "供应商",         # Ensure Maker is displayed
      MajorType = "大类",
      MinorType = "小类",
      ItemName = "商品名",
      Quantity = "库存数",
      Cost = "采购成本",
      ItemImagePath = "商品图片"
    )
    
    render_table_with_images(
      data = filtered_inventory(),
      column_mapping = column_mapping,
      image_column = "ItemImagePath",  # Specify the image column
      is_local = TRUE  # Use server-stored image paths
    )
  })
  
  # Handle row selection in filtered inventory table
  observeEvent(input$filtered_inventory_table_rows_selected, {
    selected_row <- input$filtered_inventory_table_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- filtered_inventory()[selected_row, ]
      
      # Update input fields in the sidebar
      updateSelectInput(session, "new_major_type", selected = selected_data$MajorType)
      updateSelectInput(session, "new_minor_type", selected = selected_data$MinorType)
      updateTextInput(session, "new_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = 0)
      updateNumericInput(session, "new_cost", value = selected_data$Cost)
      updateTextInput(session, "new_sku", value = selected_data$SKU)
    }
  })
  
  ---
    
    ## 入库表模块
    
    # Handle add item button click
    observeEvent(input$add_btn, {
      if (is.null(input$new_name) || input$new_name == "") {
        show_custom_notification("请填写正确商品名称！", type = "error")
        return()
      }
      
      if (is.null(input$new_quantity) || input$new_quantity == "" || input$new_quantity == 0) {
        show_custom_notification("请填写正确商品数量！", type = "error")
        return()
      }
      
      if (is.null(input$new_cost) || input$new_cost == "" || input$new_cost > 999 || input$new_cost < 0) {
        show_custom_notification("请填写正确商品成本！", type = "error")
        return()
      }
      
      if (is.null(input$new_sku) || input$new_sku == "") {
        show_custom_notification("请确保SKU正常生成！", type = "error")
        return()
      }
      
      # Check if the SKU already exists in added_items
      existing_skus <- added_items()$SKU
      if (input$new_sku %in% existing_skus) {
        show_custom_notification(paste("SKU 已存在:", input$new_sku, "无法重复添加！"), type = "error")
        return()
      }
      
      # Convert image to Base64
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
        Cost = round(input$new_cost, 2),
        ItemImage = image_data,  # Store Base64-encoded image data
        ItemImagePath = if (!is.null(input$new_item_image)) input$new_item_image$datapath else NA,
        stringsAsFactors = FALSE
      )
      
      # Update the added items reactive value
      added_items(bind_rows(added_items(), new_item))
    })
  
  ---
    
    ## 入库商品模块
    
    # Render added items table
    output$added_items_table <- renderDT({
      column_mapping <- list(
        SKU = "条形码",
        Maker = "供应商",
        MajorType = "大类",
        MinorType = "小类",
        ItemName = "商品名",
        Quantity = "入库数量",
        Cost = "采购成本",
        ItemImagePath = "商品图片"
      )
      
      render_table_with_images(
        data = added_items(),
        column_mapping = column_mapping,
        image_column = "ItemImagePath",  # Specify the image column
        is_local = TRUE  # Use server-stored image paths
      )
    })
  
  # Delete selected item
  observeEvent(input$delete_btn, {
    selected_row <- input$added_items_table_rows_selected
    if (length(selected_row) > 0) {
      current_items <- added_items()
      updated_items <- current_items[-selected_row, ]  # Remove selected row
      added_items(updated_items)  # Update reactive value
      show_custom_notification("记录已成功删除", type = "message")
    } else {
      show_custom_notification("请选择要删除的记录", type = "error")
    }
  })
  
  # Calculate total cost
  output$total_cost <- renderText({
    total <- sum(added_items()$Quantity * added_items()$Cost) + input$shipping_cost
    paste0("本次入库总金额: ¥", format(total, big.mark = ",", scientific = FALSE),
           "（其中包含运费: ¥", input$shipping_cost, ")")
  })
  
  # Confirm button: Update database and handle images
  observeEvent(input$confirm_btn, {
    if (nrow(added_items()) == 0) {
      show_custom_notification("请先添加至少一个商品再确认!", type = "error")
      return()
    }
    
    added_items_df <- added_items()
    
    # Upload images to server
    for (i in 1:nrow(added_items_df)) {
      if (!is.na(added_items_df$ItemImagePath[i])) {
        tryCatch({
          compressed_image_path <- save_compressed_image(
            file_path = added_items_df$ItemImagePath[i],
            output_dir = "/var/www/images",
            image_name = paste0(added_items_df$SKU[i], ".jpg")
          )
          added_items_df$ItemImagePath[i] <- compressed_image_path
          show_custom_notification(paste("图片压缩并上传成功! SKU:", added_items_df$SKU[i]), type = "message")
        }, error = function(e) {
          show_custom_notification(paste("图片处理失败! SKU:", added_items_df$SKU[i]), type = "error")
        })
      }
    }
    
    # Update the database
    for (i in 1:nrow(added_items_df)) {
      sku <- added_items_df$SKU[i]
      maker <- added_items_df$Maker[i]
      major_type <- added_items_df$MajorType[i]
      minor_type <- added_items_df$MinorType[i]
      item_name <- added_items_df$ItemName[i]
      quantity <- added_items_df$Quantity[i]
      cost <- added_items_df$Cost[i]
      image_path <- added_items_df$ItemImagePath[i]
      
      # Check if the SKU already exists in the inventory
      existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
      
      if (nrow(existing_item) > 0) {
        # Update existing item
        new_quantity <- existing_item$Quantity + quantity
        new_ave_cost <- ((existing_item$Cost * existing_item$Quantity) + (cost * quantity)) / new_quantity
        
        dbExecute(con, "UPDATE inventory SET 
                      Quantity = ?, Cost = ?, updated_at = NOW() 
                      WHERE SKU = ?",
                  params = list(new_quantity, round(new_ave_cost, 2), sku))
        
        # Update image path if a new image was uploaded
        if (!is.na(image_path)) {
          dbExecute(con, "UPDATE inventory SET 
                        ItemImagePath = ? 
                        WHERE SKU = ?",
                    params = list(image_path, sku))
        }
        
        show_custom_notification(paste("库存更新成功! SKU:", sku, ", 当前库存数:", new_quantity), type = "message")
      } else {
        # Insert new item
        dbExecute(con, "INSERT INTO inventory 
                      (SKU, Maker, MajorType, MinorType, ItemName, Quantity, Cost, ItemImagePath, created_at) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, NOW())",
                  params = list(sku, maker, major_type, minor_type, item_name, quantity, round(cost, 2), image_path))
        
        show_custom_notification(paste("新商品添加成功! SKU:", sku, ", 商品名:", item_name), type = "message")
      }
    }
    
    # Clear added items after confirmation
    added_items(create_empty_inventory())
  })
  
  # Automatically generate SKU when relevant inputs change
  observeEvent({
    input$new_major_type
    input$new_minor_type
    input$new_name
    input$new_maker
  }, {
    req(input$new_major_type, input$new_minor_type, input$new_name, input$new_maker)
    
    # Dynamically generate SKU
    sku <- generate_sku(
      item_type_data = item_type_data(),
      major_type = input$new_major_type,
      minor_type = input$new_minor_type,
      item_name = input$new_name,
      maker = input$new_maker
    )
    
    # Update the SKU input field
    updateTextInput(session, "new_sku", value = sku)
  })
  # 
  #   # Generate Barcode based on SKU
  #   session$onFlushed(function() {
  #     shinyjs::disable("barcode_pdf")
  #   })
  # 
  #   pdf_file_path <- reactiveVal(NULL)
  # 
  #   observeEvent(input$export_btn, {
  #     req(input$new_sku, input$new_quantity)
  #     pdf_file <- export_barcode_pdf(input$new_sku, page_width, page_height, unit = size_unit)
  #     pdf_file_path(pdf_file)
  #     show_custom_notification("条形码已导出为PDF!")
  #     shinyjs::enable("barcode_pdf")
  #   })
  # 
  #   # Download PDF button
  #   output$barcode_pdf <- downloadHandler(
  #     filename = function() {
  #       cat("Requested file path:", pdf_file_path(), "\n")  # Debugging: Check path
  #       basename(pdf_file_path())  # Use basename to just get the file name
  #     },
  #     content = function(file) {
  #       # Ensure the file exists before copying
  #       cat("Copying file from:", pdf_file_path(), "to", file, "\n")  # Debugging: Check file paths
  #       file.copy(pdf_file_path(), file, overwrite = TRUE)
  #     }
  #   )
  # 
  # 
  # 
  # 
  # 
  # observeEvent(input$reset_btn, {
  #   updateSelectizeInput(session, "new_maker", choices = maker_list()$Maker, server = TRUE)
  #   updateSelectInput(session, "new_major_type", selected = NULL)
  #   updateSelectInput(session, "new_minor_type", selected = NULL)
  #   updateTextInput(session, "new_name", value = "")
  #   updateNumericInput(session, "new_quantity", value = 1)
  #   updateNumericInput(session, "new_cost", value = 0)
  #   updateTextInput(session, "new_sku", value = "")
  #   shinyjs::reset("new_item_image")
  #   
  #   added_items(create_empty_inventory()) # 使用统一的空表函数
  #   
  #   output$filtered_inventory_table <- renderDT({
  #     column_mapping <- list(
  #       SKU = "条形码",
  #       MajorType = "大类",
  #       MinorType = "小类",
  #       ItemName = "商品名",
  #       Quantity = "库存数",
  #       Cost = "采购成本",
  #       ItemImagePath = "商品图片"
  #     )
  #     
  #     render_table_with_images(
  #       data = filtered_inventory(),
  #       column_mapping = column_mapping,
  #       image_column = "ItemImagePath",  # 指定图片列
  #       is_local = TRUE  # 服务器已有图片处理
  #     )
  #   })
  #   
  #   show_custom_notification("已重置所有输入和状态！", type = "message")
  # })
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}