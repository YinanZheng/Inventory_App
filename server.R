# Connect to MySQL Database
con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = "inventory_system",
  host = "localhost",
  user = "root",
  password = "goldenbeanllc"
)

# Define server logic
server <- function(input, output, session) {
  # Load data from MySQL
  maker_list <- reactive({
    dbGetQuery(con, "SELECT * FROM maker_list")
  })
  
  item_type_data <- reactive({
    dbGetQuery(con, "SELECT * FROM item_type_data")
  })
  
  inventory <- reactiveVal({
    dbGetQuery(con, "SELECT * FROM inventory")
  })
  
  # Reactive value to store added items
  added_items <- reactiveVal(create_empty_inventory())

  # 
  # ## 供应商模块
  # supplier_module(input, output, session, maker_sheet_id)
  # 
  
  # ## 大小类模块
  # # Render Major Type Dropdown
  # output$major_type_ui <- renderUI({
  #   type_data <- item_type_data()
  #   unique_majors <- unique(type_data[, c("MajorType", "MajorTypeSKU")])
  #   choices <- setNames(unique_majors$MajorType, paste0(unique_majors$MajorType, "（", unique_majors$MajorTypeSKU, "）"))
  #   selectInput("new_major_type", "大类:", choices = choices)
  # })
  # 
  # # Render Minor Type Dropdown dynamically
  # output$minor_type_ui <- renderUI({
  #   type_data <- item_type_data()
  #   
  #   if (!is.null(input$new_major_type)) {
  #     selected_major <- gsub("（.*）", "", input$new_major_type)
  #     # Filter rows for the selected major_type
  #     filtered_data <- type_data[type_data$MajorType == selected_major, ]
  #     choices <- setNames(filtered_data$MinorType, paste0(filtered_data$MinorType, "（", filtered_data$MinorTypeSKU, "）"))
  #     selectInput("new_minor_type", "小类:", choices = choices)
  #   }
  # })
  # 
  # 
  # ## 库存表渲染模块
  # # Filter inventory based on major and minor type
  # filtered_inventory <- reactive({
  #   req(input$new_major_type, input$new_minor_type)
  #   
  #   # 过滤结果
  #   result <- inventory() %>%
  #     filter(MajorType == input$new_major_type, MinorType == input$new_minor_type)
  #   
  #   # 如果过滤结果为空，返回空表
  #   if (nrow(result) == 0) {
  #     return(create_empty_inventory())
  #   }
  #   
  #   return(result)
  # })
  # 
  # # Render filtered inventory with column name mapping
  # output$filtered_inventory_table <- renderDT({
  #   column_mapping <- list(
  #     SKU = "条形码",
  #     MajorType = "大类",
  #     MinorType = "小类",
  #     ItemName = "商品名",
  #     Quantity = "库存数",
  #     Cost = "采购成本",
  #     ItemImagePath = "商品图片"
  #   )
  #   
  #   render_table_with_images(
  #     data = filtered_inventory(),
  #     column_mapping = column_mapping,
  #     image_column = "ItemImagePath",  # 指定图片列
  #     is_local = TRUE  # 服务器已有图片处理
  #   )
  # })
  # 
  # # Handle row selection in filtered inventory table
  # observeEvent(input$filtered_inventory_table_rows_selected, {
  #   selected_row <- input$filtered_inventory_table_rows_selected
  #   if (length(selected_row) > 0) {
  #     selected_data <- filtered_inventory()[selected_row, ]
  #     
  #     # Update the input fields in the sidebar
  #     updateSelectInput(session, "new_major_type", selected = selected_data$MajorType)
  #     updateSelectInput(session, "new_minor_type", selected = selected_data$MinorType)
  #     updateTextInput(session, "new_name", value = selected_data$ItemName)
  #     updateNumericInput(session, "new_quantity", value = 0)
  #     updateNumericInput(session, "new_cost", value = selected_data$Cost)
  #     updateTextInput(session, "new_sku", value = selected_data$SKU)
  #   }
  # })
  # 
  # 
  # ## 入库表渲染模块
  # # Handle add item button click
  # observeEvent(input$add_btn, {
  #   if (is.null(input$new_name) || input$new_name == "") {
  #     show_custom_notification("请填写正确商品名称！", type = "error")
  #     return()
  #   }
  #   
  #   if (is.null(input$new_quantity) || input$new_quantity == "" || input$new_quantity == 0) {
  #     show_custom_notification("请填写正确商品数量！", type = "error")
  #     return()
  #   }
  #   
  #   if (is.null(input$new_cost) || input$new_cost == "" || input$new_cost > 999 || input$new_cost < 0) {
  #     show_custom_notification("请填写正确商品成本！", type = "error")
  #     return()
  #   }
  #   
  #   if (is.null(input$new_sku) || input$new_sku == "") {
  #     show_custom_notification("请确保SKU正常生成！", type = "error")
  #     return()
  #   }
  #   
  #   # 检查 SKU 是否已存在于 added_items 中
  #   existing_skus <- added_items()$SKU
  #   if (input$new_sku %in% existing_skus) {
  #     show_custom_notification(paste("SKU 已存在:", input$new_sku, "无法重复添加！"), type = "error")
  #     return()
  #   }
  #   
  #   # 转换图片为 Base64
  #   image_data <- if (!is.null(input$new_item_image)) {
  #     base64enc::dataURI(file = input$new_item_image$datapath, mime = input$new_item_image$type)
  #   } else {
  #     NA
  #   }
  #   
  #   new_item <- data.frame(
  #     SKU = input$new_sku,
  #     Maker = input$new_maker,
  #     MajorType = input$new_major_type,
  #     MinorType = input$new_minor_type,
  #     ItemName = input$new_name,
  #     Quantity = input$new_quantity,
  #     Cost = round(input$new_cost),
  #     ItemImage = image_data,  # 存储 Base64 编码的图片数据
  #     ItemImagePath = if (!is.null(input$new_item_image)) input$new_item_image$datapath else NA, 
  #     stringsAsFactors = FALSE
  #   )
  #   
  #   # Update the added items reactive value
  #   added_items(bind_rows(added_items(), new_item))
  # })
  # 
  # # Render added items table
  # output$added_items_table <- renderDT({
  #   column_mapping <- list(
  #     SKU = "条形码",
  #     Maker = "供应商",
  #     MajorType = "大类",
  #     MinorType = "小类",
  #     ItemName = "商品名",
  #     Quantity = "入库数量",
  #     Cost = "采购成本",
  #     ItemImage = "商品图片"
  #   )
  #   
  #   render_table_with_images(
  #     data = added_items(),
  #     column_mapping = column_mapping,
  #     image_column = "ItemImage",  # 指定图片列
  #     is_local = FALSE  # URL 或 Base64 图片处理
  #   )
  # })
  # 
  # # Delete selected item
  # observeEvent(input$delete_btn, {
  #   selected_row <- input$added_items_table_rows_selected
  #   if (length(selected_row) > 0) {
  #     current_items <- added_items()
  #     updated_items <- current_items[-selected_row, ]  # Remove selected row
  #     added_items(updated_items)  # Update reactive value
  #     show_custom_notification("记录已成功删除", type = "message")
  #   } else {
  #     show_custom_notification("请选择要删除的记录", type = "error")
  #   }
  # })
  # 
  # output$total_cost <- renderText({
  #   total <- sum(added_items()$Quantity * added_items()$Cost) + input$shipping_cost
  #   paste0("本次入库总金额: ¥", format(total, big.mark = ",", scientific = FALSE), 
  #          "（其中包含运费: ¥", input$shipping_cost, ")")
  # })
  # 
  # # # Handle confirm button click to update the database
  # # observeEvent(input$confirm_btn, {
  # #   # If no items are added, show an error notification
  # #   if (nrow(added_items()) == 0) {
  # #     show_custom_notification("请先添加至少一个商品再确认!", type = "error")
  # #     return()
  # #   }
  # #   
  # #   added_items_df <- added_items()
  # #   
  # #   # Upload images to Google Drive
  # #   for (i in 1:nrow(added_items_df)) {
  # #     if (!is.na(added_items_df$ItemImagePath[i])) {
  # #       tryCatch({
  # #         drive_file <- save_image_to_drive(added_items_df$ItemImagePath[i], images_folder_id, added_items_df$SKU[i])
  # #         #Update image path to google drive path
  # #         added_items_df$ItemImagePath[i] <- drive_file$id
  # #         show_custom_notification(paste("图片上传成功! SKU:", added_items_df$SKU[i], ", 商品名:", added_items_df$ItemName[i]), type = "message")
  # #       }, error = function(e) {
  # #         show_custom_notification(paste("图片上传失败! SKU:", added_items_df$SKU[i], ", 商品名:", added_items_df$ItemName[i]), type = "error")
  # #       })
  # #     }
  # #   }
  # #   
  # #   # Update the Google Sheet with the added items (excluding Maker and ItemImage)
  # #   # Loop through added items to either update or add
  # #   for (i in 1:nrow(added_items_df)) {
  # #     sku <- added_items_df$SKU[i]
  # #     
  # #     if (sku %in% inventory()$SKU) {
  # #       # Update existing item quantity in Google Sheets
  # #       sheet_range <- which(inventory()$SKU == sku)
  # #       if (length(sheet_range) == 1) {
  # #         inventory_quantity <- ifelse(is.na(inventory()$Quantity[sheet_range]), 0, as.integer(inventory()$Quantity[sheet_range]))
  # #         added_quantity <- ifelse(is.na(added_items_df$Quantity[i]), 0, as.integer(added_items_df$Quantity[i]))
  # #         updated_quantity <- inventory_quantity + added_quantity
  # #         range_write(
  # #           ss = inventory_sheet_id, 
  # #           data = data.frame(updated_quantity), 
  # #           sheet = "Sheet1", 
  # #           range = paste0("E", sheet_range + 1), 
  # #           col_names = FALSE 
  # #         )
  # #         
  # #         # Update Image if new image uploaded
  # #         if (!is.na(added_items_df$ItemImagePath[i])) {
  # #           range_write(
  # #             ss = inventory_sheet_id,
  # #             data = data.frame(added_items_df$ItemImagePath[i]),
  # #             sheet = "Sheet1",
  # #             range = paste0("G", sheet_range + 1),
  # #             col_names = FALSE
  # #           )
  # #           show_custom_notification(paste("图片更新成功! SKU:", sku, ", 商品名:", added_items_df$ItemName[i]), type = "message")
  # #         }
  # #         show_custom_notification(paste("库存更新成功! SKU:", sku, ", 当前库存数:", updated_quantity), type = "message")
  # #       } else {
  # #         show_custom_notification(paste("找到多条记录SKU:", sku), type = "error")
  # #       }
  # #     } else {
  # #       # Add new item
  # #       sheet_append(ss = inventory_sheet_id, data = added_items_df[i, ] %>% select(-Maker, -ItemImage), sheet = "Sheet1")
  # #       show_custom_notification(paste("新商品添加成功! SKU:", sku, ", 商品名:", added_items_df$ItemName[i]), type = "message")
  # #     }
  # #   }
  # #   
  # #   inventory(read_sheet(inventory_sheet_id)) # Refresh the inventory to reflect any updates
  # #   
  # #   # Clear the added items after confirming
  # #   added_items(create_empty_inventory())
  # # })
  # 
  # 
  # # Generate Barcode based on SKU
  # session$onFlushed(function() {
  #   shinyjs::disable("barcode_pdf")
  # })
  # 
  # pdf_file_path <- reactiveVal(NULL)
  # 
  # observeEvent(input$export_btn, {
  #   req(input$new_sku, input$new_quantity)
  #   pdf_file <- export_barcode_pdf(input$new_sku, page_width, page_height, unit = size_unit)
  #   pdf_file_path(pdf_file)
  #   show_custom_notification("条形码已导出为PDF!")
  #   shinyjs::enable("barcode_pdf")
  # })
  # 
  # # Download PDF button
  # output$barcode_pdf <- downloadHandler(
  #   filename = function() {
  #     cat("Requested file path:", pdf_file_path(), "\n")  # Debugging: Check path
  #     basename(pdf_file_path())  # Use basename to just get the file name
  #   },
  #   content = function(file) {
  #     # Ensure the file exists before copying
  #     cat("Copying file from:", pdf_file_path(), "to", file, "\n")  # Debugging: Check file paths
  #     file.copy(pdf_file_path(), file, overwrite = TRUE)
  #   }
  # )
  # 
  # # Automatically generate SKU when relevant inputs change
  # observeEvent({input$new_cost; input$new_major_type; input$new_minor_type; input$new_name}, {
  #   req(input$new_major_type, input$new_minor_type, input$new_name, input$new_cost)
  #   
  #   
  #   sku <- generate_sku(item_type_data(),
  #                       input$new_major_type,
  #                       input$new_minor_type,
  #                       input$new_name,
  #                       input$new_cost)
  #   updateTextInput(session, "new_sku", value = sku)
  #   
  # })
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