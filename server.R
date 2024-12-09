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
  
  inventory <- reactiveVal({
    dbGetQuery(con, "SELECT * FROM inventory")
  })
  
  # Reactive value to store added items
  added_items <- reactiveVal(create_empty_inventory())
  
  
  ## 供应商模块
  supplier_module(input, output, session, con)
  
  
  ## 大小类模块
  item_type_data <- reactive({
    tryCatch({
      dbGetQuery(con, "SELECT * FROM item_type_data")
    }, error = function(e) {
      log_debug(paste("Error fetching item_type_data:", e$message))
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
  
  # Filter inventory based on major and minor type
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
      Cost = "平均成本",
      ItemImagePath = "商品图片"
    )
    
    render_table_with_images(
      data = filtered_inventory(),
      column_mapping = column_mapping,
      image_column = "ItemImagePath"  # Specify the image column
    )
  })
  
  # Handle row selection in filtered inventory table
  observeEvent(input$filtered_inventory_table_rows_selected, {
    selected_row <- input$filtered_inventory_table_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- filtered_inventory()[selected_row, ]
      
      # Update input fields in the sidebar
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "new_major_type", selected = selected_data$MajorType)
      updateSelectInput(session, "new_minor_type", selected = selected_data$MinorType)
      updateTextInput(session, "new_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = 0)
      updateNumericInput(session, "new_cost", value = selected_data$Cost)
    }
    shinyjs::reset("new_item_image")  # 重置文件上传控件
  })
  
  ## 入库表模块
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    # 验证输入
    if (is.null(input$new_name) || input$new_name == "") {
      show_custom_notification("请填写正确商品名称！", type = "error")
      return()
    }
    
    if (is.null(input$new_quantity) || input$new_quantity <= 0) {
      show_custom_notification("请填写正确商品数量！", type = "error")
      return()
    }
    
    if (is.null(input$new_cost) || input$new_cost < 0 || input$new_cost > 999) {
      show_custom_notification("请填写正确商品成本！", type = "error")
      return()
    }
    
    if (is.null(input$new_sku) || input$new_sku == "") {
      show_custom_notification("请确保SKU正常生成！", type = "error")
      return()
    }
    
    # 检查是否存在重复的 SKU
    existing_items <- added_items()
    existing_skus <- existing_items$SKU
    
    if (input$new_sku %in% existing_skus) {
      # SKU 已存在，执行覆盖更新操作
      sku_index <- which(existing_skus == input$new_sku)
      
      # 初始化图片路径
      updated_image_path <- existing_items$ItemImagePath[sku_index]
      
      # 如果上传了图片，更新图片路径；否则保持原路径
      if (!is.null(input$new_item_image)) {
        tryCatch({
          # 为图片生成唯一文件名
          unique_image_name <- paste0(input$new_sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
          output_dir <- "/var/www/images"
          final_image_path <- file.path(output_dir, unique_image_name)
          
          # 保存压缩后的图片
          save_compressed_image(
            file_path = input$new_item_image$datapath,
            output_dir = output_dir,
            image_name = unique_image_name
          )
          updated_image_path <- final_image_path
          show_custom_notification("图片已成功更新并保存！", type = "message")
        }, error = function(e) {
          show_custom_notification("图片更新失败！", type = "error")
          log_debug(paste("Error in add_btn:", e$message))
        })
      } 
      
      # 覆盖更新记录
      existing_items[sku_index, ] <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input$new_major_type,
        MinorType = input$new_minor_type,
        ItemName = input$new_name,
        Quantity = input$new_quantity,
        Cost = round(input$new_cost, 2),
        ItemImagePath = updated_image_path,
        stringsAsFactors = FALSE
      )
      
      added_items(existing_items)
      
      show_custom_notification(paste("SKU 已更新:", input$new_sku, "已覆盖旧记录"), type = "message")
    } else {
      # SKU 不存在，添加新记录
      new_image_path <- NA
      if (!is.null(input$new_item_image)) {
        tryCatch({
          unique_image_name <- paste0(input$new_sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
          output_dir <- "/var/www/images"
          final_image_path <- file.path(output_dir, unique_image_name)
          
          save_compressed_image(
            file_path = input$new_item_image$datapath,
            output_dir = output_dir,
            image_name = unique_image_name
          )
          new_image_path <- final_image_path
          show_custom_notification("图片已成功处理并保存！", type = "message")
        }, error = function(e) {
          show_custom_notification("图片处理失败！", type = "error")
        })
      }
      
      new_item <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input$new_major_type,
        MinorType = input$new_minor_type,
        ItemName = input$new_name,
        Quantity = input$new_quantity,
        Cost = round(input$new_cost, 2),
        ItemImagePath = new_image_path,
        stringsAsFactors = FALSE
      )
      
      added_items(bind_rows(existing_items, new_item))
      show_custom_notification(paste("SKU 已添加:", input$new_sku, "商品名:", input$new_name), type = "message")
    }
    shinyjs::reset("new_item_image")  # 重置文件上传控件
  })
  
  
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
      image_column = "ItemImagePath"  # Specify the correct image column
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
    
    for (i in 1:nrow(added_items_df)) {
      sku <- added_items_df$SKU[i]
      maker <- added_items_df$Maker[i]
      major_type <- added_items_df$MajorType[i]
      minor_type <- added_items_df$MinorType[i]
      item_name <- added_items_df$ItemName[i]
      quantity <- added_items_df$Quantity[i]
      cost <- added_items_df$Cost[i]
      new_image_path <- added_items_df$ItemImagePath[i]
      
      # 检查 SKU 是否已存在
      existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
      
      # 如果 SKU 已存在，检查图片路径
      if (nrow(existing_item) > 0) {
        new_quantity <- existing_item$Quantity + quantity
        new_ave_cost <- ((existing_item$Cost * existing_item$Quantity) + (cost * quantity)) / new_quantity
        
        # 如果有新图片上传，为图片生成唯一路径
        if (!is.na(new_image_path) && new_image_path != "") {
          unique_image_name <- paste0(sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
          final_image_path <- file.path("/var/www/images", unique_image_name)
          
          tryCatch({
            # 保存新图片
            file.rename(new_image_path, final_image_path)
            new_image_path <- final_image_path
          }, error = function(e) {
            show_custom_notification(paste("图片保存失败! SKU:", sku), type = "error")
            new_image_path <- existing_item$ItemImagePath  # 回退为原始路径
            log_debug(paste("Error in confirm_btn(file.rename):", e$message))
          })
        } else {
          # 未上传新图片，保留现有图片路径
          new_image_path <- existing_item$ItemImagePath
        }
        
        # 更新库存数据
        dbExecute(con, "UPDATE inventory 
                      SET Quantity = ?, Cost = ?, ItemImagePath = ?, updated_at = NOW() 
                      WHERE SKU = ?",
                  params = list(new_quantity, round(new_ave_cost, 2), new_image_path, sku))
        
        show_custom_notification(paste("库存更新成功! SKU:", sku, ", 当前库存数:", new_quantity), type = "message")
      } else {
        # 如果 SKU 不存在，插入新商品
        unique_image_name <- if (!is.na(new_image_path) && new_image_path != "") {
          paste0(sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        } else {
          NA
        }
        
        if (!is.na(unique_image_name)) {
          final_image_path <- file.path("/var/www/images", unique_image_name)
          
          tryCatch({
            # 保存新图片
            file.rename(new_image_path, final_image_path)
            new_image_path <- final_image_path
          }, error = function(e) {
            show_custom_notification(paste("新商品图片保存失败! SKU:", sku), type = "error")
            log_debug(paste("Error in confirm_btn:", e$message))
          })
        }
        
        dbExecute(con, "INSERT INTO inventory 
                      (SKU, Maker, MajorType, MinorType, ItemName, Quantity, Cost, ItemImagePath, created_at, updated_at) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())",
                  params = list(sku, maker, major_type, minor_type, item_name, quantity, round(cost, 2), new_image_path))
        
        show_custom_notification(paste("新商品添加成功! SKU:", sku, ", 商品名:", item_name), type = "message")
      }
    }
    
    # 刷新库存数据
    inventory({
      dbGetQuery(con, "SELECT * FROM inventory")
    })
    
    show_custom_notification("库存已成功更新！", type = "message")
    

    # Prepare data for batch insertion
    batch_data <- do.call(rbind, lapply(1:nrow(added_items_df), function(i) {
      sku <- added_items_df$SKU[i]
      quantity <- added_items_df$Quantity[i]
      cost <- added_items_df$Cost[i]
      
      if (quantity <= 0 || is.na(cost) || cost <= 0) {
        return(NULL)
      }
      
      replicate(quantity, c(
        UUIDgenerate(),
        as.character(sku),
        as.numeric(cost),
        "国内仓入库",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ), simplify = TRUE) |> t()
    }))
    
    if (is.null(batch_data) || nrow(batch_data) == 0) {
      show_custom_notification("批量数据无效，请检查输入。", type = "error")
      return()
    }
    
    batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)
    names(batch_data) <- c("UniqueID", "SKU", "Cost", "Status", "DomesticEntryTime")
    
    # Ensure all SKUs exist in inventory
    existing_skus <- dbGetQuery(con, "SELECT SKU FROM inventory")$SKU
    missing_skus <- setdiff(added_items_df$SKU, existing_skus)
    
    if (length(missing_skus) > 0) {
      show_custom_notification(paste("以下 SKU 不存在:", toString(missing_skus)), type = "error")
      return()
    }
    
    # Insert all records
    dbBegin(con)
    tryCatch({
      dbExecute(con, "
    INSERT INTO unique_items (UniqueID, SKU, Cost, Status, DomesticEntryTime) 
    VALUES (?, ?, ?, ?, ?)",
                params = unname(as.list(as.matrix(batch_data)))
      )
      dbCommit(con)
      show_custom_notification("所有物品已成功入库到国内仓！", type = "message")
    }, error = function(e) {
      dbRollback(con)
      show_custom_notification(paste("批量入库失败:", e$message), type = "error")
    })
    
    
    # Clear added items and reset input fields
    added_items(create_empty_inventory())
    if (!is.null(input$new_item_image)) {
      shinyjs::reset("new_item_image")
    }
  })
  
  # Handle row selection in item table
  observeEvent(input$added_items_table_rows_selected, {
    selected_row <- input$added_items_table_rows_selected
    if (length(selected_row) > 0) {
      selected_data <- added_items()[selected_row, ]
      
      # Update input fields in the sidebar
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "new_major_type", selected = selected_data$MajorType)
      updateSelectInput(session, "new_minor_type", selected = selected_data$MinorType)
      updateTextInput(session, "new_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = selected_data$Quantity)
      updateNumericInput(session, "new_cost", value = selected_data$Cost)
    }
  })
  
  
  
  ## 物品追踪表
  unique_items_data <- reactive({
    dbGetQuery(con, "
    SELECT 
      unique_items.UniqueID AS '唯一物品编码',
      unique_items.Status AS '当前状态',
      unique_items.DomesticEntryTime AS '国内仓入库时间',
      unique_items.DomesticExitTime AS '国内仓出库时间',
      unique_items.UsEntryTime AS '美国仓入库时间',
      unique_items.UsExitTime AS '美国仓出库时间',
      inventory.Maker AS '供应商',
      inventory.MajorType AS '大类',
      inventory.MinorType AS '小类',
      inventory.ItemName AS '商品名',
      inventory.ItemImagePath AS '商品图片'
    FROM 
      unique_items
    JOIN 
      inventory 
    ON 
      unique_items.SKU = inventory.SKU
    ORDER BY 
      unique_items.DomesticEntryTime DESC
  ")
  })
  
  # 渲染 unique_items 数据表
  # output$unique_items_table <- renderDT({
  #   # Define column mapping for user-friendly display
  #   column_mapping <- list(
  #     UniqueID = "唯一物品编码",
  #     Status = "当前状态",
  #     DomesticEntryTime = "国内仓入库时间",
  #     DomesticExitTime = "国内仓出库时间",
  #     UsEntryTime = "美国仓入库时间",
  #     UsExitTime = "美国仓出库时间",
  #     Maker = "供应商",
  #     MajorType = "大类",
  #     MinorType = "小类",
  #     ItemName = "商品名",
  #     ItemImagePath = "商品图片"
  #   )
  #   
  #   # Render table with images
  #   render_table_with_images(
  #     data = unique_items_data(),     # Use the reactive data source
  #     column_mapping = column_mapping, # Map columns to user-friendly names
  #     image_column = "ItemImagePath"   # Specify the column for images
  #   )
  # })
  # 
  output$unique_items_table <- renderDT({
    data <- unique_items_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(datatable(data.frame("消息" = "没有数据可显示"), escape = FALSE))
    }
    
    datatable(data)
  })
  
  
  
  
  
  ### SKU Barcode 模块
  
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
  
  # Generate Barcode based on SKU
  session$onFlushed(function() {
    shinyjs::disable("barcode_pdf")
  })
  
  pdf_file_path <- reactiveVal(NULL)
  
  observeEvent(input$export_btn, {
    req(input$new_sku, input$new_quantity)
    pdf_file <- export_barcode_pdf(input$new_sku, page_width, page_height, unit = size_unit)
    pdf_file_path(pdf_file)
    show_custom_notification("条形码已导出为PDF!")
    shinyjs::enable("barcode_pdf")
  })
  
  # Download PDF button
  output$barcode_pdf <- downloadHandler(
    filename = function() {
      cat("Requested file path:", pdf_file_path(), "\n")  # Debugging: Check path
      basename(pdf_file_path())  # Use basename to just get the file name
    },
    content = function(file) {
      # Ensure the file exists before copying
      cat("Copying file from:", pdf_file_path(), "to", file, "\n")  # Debugging: Check file paths
      file.copy(pdf_file_path(), file, overwrite = TRUE)
    }
  )
  
  
  
  
  
  observeEvent(input$reset_btn, {
    tryCatch({
      # 清空输入控件
      updateSelectizeInput(session, "new_maker", selected = NULL)  # 清空供应商选择
      updateSelectInput(session, "new_major_type", selected = NULL)  # 清空大类选择
      updateSelectInput(session, "new_minor_type", selected = NULL)  # 清空小类选择
      updateTextInput(session, "new_name", value = "")  # 清空商品名
      updateNumericInput(session, "new_quantity", value = 1)  # 恢复数量默认值
      updateNumericInput(session, "new_cost", value = 0)  # 恢复成本默认值
      updateNumericInput(session, "shipping_cost", value = 0)  # 恢复运费默认值
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      shinyjs::reset("new_item_image")  # 重置文件上传控件
      
      # 清空已添加的商品
      added_items(create_empty_inventory())
      
      # 通知用户
      show_custom_notification("输入已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      show_custom_notification("清空输入时发生错误，请重试！", type = "error")
      log_debug(paste("Error in reset_btn:", e$message))
    })
  })
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}