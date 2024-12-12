# Define server logic
server <- function(input, output, session) {
  # Database
  con <- db_connection()
  
  # ReactiveVal 存储 item_type_data 数据
  item_type_data <- reactiveVal()
  
  # ReactiveVal 用于存储 inventory 数据
  inventory <- reactiveVal()
  
  # 用于保存用户上传的文件信息
  uploaded_file <- reactiveVal(NULL)
  
  # 声明一个 reactiveVal 用于触发表格刷新
  refresh_trigger <- reactiveVal(FALSE)
  
  # Reactive value to store added items
  added_items <- reactiveVal(create_empty_inventory() %>% select(-ShippingCost))
  
  # 用于存储 PDF 文件路径
  single_pdf_file_path <- reactiveVal(NULL)
  batch_pdf_file_path <- reactiveVal(NULL)
  
  # 存储条形码是否已生成的状态
  barcode_generated <- reactiveVal(FALSE)  # 初始化为 FALSE
  
  # 用于存储撤回记录的队列
  undo_queue <- reactiveVal(list())  # 每次出库时记录 UniqueID 和状态
  
  ###############################################################################################################
  
  # 应用启动时加载数据
  observe({
    tryCatch({
      data <- dbGetQuery(con, "SELECT * FROM item_type_data")
      item_type_data(data)
    }, error = function(e) {
      item_type_data(NULL)  # 如果出错，设为空值
      showNotification("Failed to load item type data.", type = "error")
    })
  })
  
  # 应用启动时加载数据
  observe({
    tryCatch({
      data <- dbGetQuery(con, "SELECT * FROM inventory")
      inventory(data)  # 存储到 reactiveVal
    }, error = function(e) {
      inventory(NULL)  # 如果失败，设为空
      showNotification("Failed to load inventory data.", type = "error")
    })
  })
  
  
  ################################################################
  ##                                                            ##
  ## 供应商模块                                                 ##
  ##                                                            ##
  ################################################################

  supplierModuleServer(input, output, session, con)

  
  
  ################################################################
  ##                                                            ##
  ## 物品大小类模块                                             ##
  ##                                                            ##
  ################################################################

  typeModuleServer("type_module", con, item_type_data)
  
  
  
  #########################################################################
  
  

  observeEvent(input$new_item_image, {
    if (!is.null(input$new_item_image)) {
      # 记录新上传的文件
      uploaded_file(input$new_item_image)
      showNotification("文件已上传并记录！", type = "message")
    }
  })
  
  observeEvent(input$toggle_inventory_table, {
    shinyjs::toggle("inventory_table_container")  # 切换显示/隐藏
  })
  
  observeEvent(input$toggle_item_table, {
    shinyjs::toggle("item_table_container")  # 切换显示/隐藏
  })
  
  

  
  ## 库存表渲染模块
  
  # Filter inventory based on major and minor type
  filtered_inventory <- reactive({
    # 当 refresh_trigger 改变时触发更新
    refresh_trigger()
    
    req(input[["type_module-new_major_type"]], input[["type_module-new_minor_type"]])
    
    # Filter the inventory data
    result <- inventory() %>%
      filter(MajorType == input[["type_module-new_major_type"]], MinorType == input[["type_module-new_minor_type"]]) %>%
      select(SKU, Maker, MajorType, MinorType, ItemName, Quantity, ProductCost, ShippingCost, ItemImagePath)  # Ensure Maker is included
    
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
      ItemName = "商品名",
      ItemImagePath = "商品图片",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "总库存数",
      ProductCost = "平均成本",
      ShippingCost = "平均运费"
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
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost)
    }
  })
  
  ## 入库表模块
  
  # Handle add item button click
  observeEvent(input$add_btn, {
    # 验证输入
    if (is.null(input$new_name) || input$new_name == "") {
      showNotification("请填写正确商品名称！", type = "error")
      return()
    }
    
    if (is.null(input$new_quantity) || input$new_quantity <= 0) {
      showNotification("请填写正确商品数量！", type = "error")
      return()
    }
    
    if (is.null(input$new_product_cost) || input$new_product_cost < 0 || input$new_product_cost > 999) {
      showNotification("请填写正确商品成本！", type = "error")
      return()
    }
    
    if (is.null(input$new_sku) || input$new_sku == "") {
      showNotification("请确保SKU正常生成！", type = "error")
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
      if (!is.null(uploaded_file())) {
        tryCatch({
          file_data <- uploaded_file()
          # 为图片生成唯一文件名
          unique_image_name <- paste0(input$new_sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
          output_dir <- "/var/www/images"
          final_image_path <- file.path(output_dir, unique_image_name)
          
          # 保存压缩后的图片
          save_compressed_image(
            file_path = file_data$datapath,
            output_dir = output_dir,
            image_name = unique_image_name
          )
          updated_image_path <- final_image_path
          showNotification("图片已成功更新并保存！", type = "message")
        }, error = function(e) {
          showNotification("图片更新失败！", type = "error")
        })
      } 
      
      # 覆盖更新记录
      existing_items[sku_index, ] <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input[["type_module-new_major_type"]],
        MinorType = input[["type_module-new_minor_type"]],
        ItemName = input$new_name,
        Quantity = input$new_quantity,
        ProductCost = round(input$new_product_cost, 2),
        ItemImagePath = updated_image_path,
        stringsAsFactors = FALSE
      )
      
      added_items(existing_items)
      
      showNotification(paste("SKU 已更新:", input$new_sku, "已覆盖旧记录"), type = "message")
    } else {
      # SKU 不存在，添加新记录
      new_image_path <- NA
      if (!is.null(uploaded_file())) {
        tryCatch({
          file_data <- uploaded_file()
          unique_image_name <- paste0(input$new_sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
          output_dir <- "/var/www/images"
          final_image_path <- file.path(output_dir, unique_image_name)
          
          save_compressed_image(
            file_path = file_data$datapath,
            output_dir = output_dir,
            image_name = unique_image_name
          )
          new_image_path <- final_image_path
          showNotification("图片已成功处理并保存！", type = "message")
        }, error = function(e) {
          showNotification("图片处理失败！", type = "error")
        })
      } else {
        # 用户未上传图片，检查 inventory 中是否有对应 SKU
        tryCatch({
          inventory_item <- dbGetQuery(con, "SELECT ItemImagePath FROM inventory WHERE SKU = ?", params = list(input$new_sku))
          
          if (nrow(inventory_item) > 0) {
            new_image_path <- inventory_item$ItemImagePath[1]  # 使用 inventory 中的图片路径
            showNotification("使用库存中 SKU 对应的图片路径！", type = "message")
          } else {
            showNotification("未找到库存中对应 SKU 的图片路径。", type = "warning")
          }
        }, error = function(e) {
          showNotification("检查库存时发生错误！", type = "error")
        })
      }
      
      new_item <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input[["type_module-new_major_type"]],
        MinorType = input[["type_module-new_minor_type"]],
        ItemName = input$new_name,
        Quantity = input$new_quantity,
        ProductCost = round(input$new_product_cost, 2),
        ItemImagePath = new_image_path,
        stringsAsFactors = FALSE
      )
      
      added_items(bind_rows(existing_items, new_item))
      showNotification(paste("SKU 已添加:", input$new_sku, "商品名:", input$new_name), type = "message")
    }
    
    # 重置文件输入框
    shinyjs::reset("new_item_image")
    uploaded_file(NULL)
  })
  
  
  ## 入库商品模块
  
  # Render added items table
  output$added_items_table <- renderDT({
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图片",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Quantity = "入库数量",
      ProductCost = "采购成本"
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
      showNotification("记录已成功删除", type = "message")
    } else {
      showNotification("请选择要删除的记录", type = "error")
    }
  })
  
  # Calculate total cost
  output$total_cost <- renderText({
    total <- sum(added_items()$Quantity * added_items()$ProductCost) + input$new_shipping_cost
    paste0("本次入库总金额: ¥", format(total, big.mark = ",", scientific = FALSE),
           "（其中包含运费: ¥", input$new_shipping_cost, ")")
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
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost)
    }
  })
  
  
  ####################################################################
  
  # Confirm button: Update database and handle images
  observeEvent(input$confirm_btn, {
    tryCatch({
      
      if (nrow(added_items()) == 0) {
        showNotification("请先添加至少一个商品再确认!", type = "error")
        return()
      }
      
      added_items_df <- added_items()
      
      # Retrieve total package shipping cost from the UI
      total_shipping_cost <- input$new_shipping_cost
      if (is.null(total_shipping_cost) || total_shipping_cost <= 0) {
        total_shipping_cost <- 0  # Default to 0 if invalid
      }
      
      unit_shipping_cost <- total_shipping_cost / sum(added_items_df$Quantity)
      
      for (i in 1:nrow(added_items_df)) {
        sku <- added_items_df$SKU[i]
        maker <- added_items_df$Maker[i]
        major_type <- added_items_df$MajorType[i]
        minor_type <- added_items_df$MinorType[i]
        item_name <- added_items_df$ItemName[i]
        quantity <- added_items_df$Quantity[i]
        product_cost <- added_items_df$ProductCost[i]
        new_image_path <- added_items_df$ItemImagePath[i]
        
        # 检查 SKU 是否已存在
        existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
        
        # 如果 SKU 已存在，检查图片路径
        if (nrow(existing_item) > 0) {
          new_quantity <- existing_item$Quantity + quantity
          new_ave_product_cost <- ((existing_item$ProductCost * existing_item$Quantity) + (product_cost * quantity)) / new_quantity
          new_ave_shipping_cost <- ((existing_item$ShippingCost * existing_item$Quantity) + (unit_shipping_cost * quantity)) / new_quantity
          
          # 如果有新图片上传，为图片生成唯一路径
          if (!is.na(new_image_path) && new_image_path != "") {
            unique_image_name <- paste0(sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
            final_image_path <- file.path("/var/www/images", unique_image_name)
            
            tryCatch({
              # 保存新图片
              file.rename(new_image_path, final_image_path)
              new_image_path <- final_image_path
            }, error = function(e) {
              showNotification(paste("图片保存失败! SKU:", sku), type = "error")
              new_image_path <- existing_item$ItemImagePath  # 回退为原始路径
            })
          } else {
            # 未上传新图片，保留现有图片路径
            new_image_path <- existing_item$ItemImagePath
          }
          
          # 更新库存数据
          dbExecute(con, "UPDATE inventory 
                      SET Quantity = ?, ProductCost = ?, ShippingCost = ?, ItemImagePath = ?, updated_at = NOW() 
                      WHERE SKU = ?",
                    params = list(new_quantity, round(new_ave_product_cost, 2), round(new_ave_shipping_cost, 2), new_image_path, sku))
          
          showNotification(paste("库存更新成功! SKU:", sku, ", 当前库存数:", new_quantity), type = "message")
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
              showNotification(paste("新商品图片保存失败! SKU:", sku), type = "error")
            })
          }
          
          dbExecute(con, "INSERT INTO inventory 
                      (SKU, Maker, MajorType, MinorType, ItemName, Quantity, ProductCost, ShippingCost, ItemImagePath, created_at, updated_at) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())",
                    params = list(sku, maker, major_type, minor_type, item_name, quantity, 
                                  round(product_cost, 2), round(unit_shipping_cost, 2), new_image_path))
          
          showNotification(paste("新商品添加成功! SKU:", sku, ", 商品名:", item_name), type = "message")
        }
      }
      
      # 刷新库存数据
      inventory({dbGetQuery(con, "SELECT * FROM inventory")})
      
      showNotification("库存已成功更新！", type = "message")
      
      
      ### 同时添加信息到 unique_items 表中
      # Prepare data for batch insertion
      batch_data <- do.call(rbind, lapply(1:nrow(added_items_df), function(i) {
        sku <- added_items_df$SKU[i]
        quantity <- added_items_df$Quantity[i]
        product_cost <- added_items_df$ProductCost[i]
        
        # Create rows for each quantity
        t(replicate(quantity, c(
          UUIDgenerate(),
          as.character(sku),
          as.numeric(product_cost),
          as.numeric(unit_shipping_cost),
          "国内入库",
          "无瑕",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
      }))
      
      # Validate data
      if (is.null(batch_data) || nrow(batch_data) == 0) {
        showNotification("批量数据无效，请检查输入！", type = "error")
        return()
      }
      
      # Convert to data frame
      batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)
      # colnames(batch_data) <- c("UniqueID", "SKU", "ProductCost", "DomesticShippingCost", "Status", "Defect", "DomesticEntryTime")
      
      # Insert into database
      dbBegin(con)
      tryCatch({
        # Insert rows one by one
        for (i in 1:nrow(batch_data)) {
          # Ensure the parameters are passed as an unnamed vector
          dbExecute(con, "
      INSERT INTO unique_items (UniqueID, SKU, ProductCost, DomesticShippingCost, Status, Defect, DomesticEntryTime) 
      VALUES (?, ?, ?, ?, ?, ?, ?)",
                    unname(as.vector(batch_data[i, ]))  # Use `unname` to avoid named parameters
          )
        }
        dbCommit(con)  # Commit transaction
        showNotification("所有物品已成功入库到国内仓！", type = "message")
        # 切换触发器值，确保刷新
        current_value <- refresh_trigger()
        refresh_trigger(!current_value) 
      }, error = function(e) {
        dbRollback(con)  # Rollback on error
        showNotification(paste("批量入库失败:", e$message), type = "error")
      })
      
      # Clear added items and reset input fields
      added_items(create_empty_inventory())
      
      # 重置文件输入框
      shinyjs::reset("new_item_image")
      uploaded_file(NULL)
    }, error = function(e) {
      # Catch and display errors
      showNotification(paste("发生错误:", e$message), type = "error")
    })
  })
  
  
  ####################################################################
  
  
  ## 物品追踪表
  unique_items_data <- reactive({
    # 当 refresh_trigger 改变时触发更新
    refresh_trigger()
    
    dbGetQuery(con, "
    SELECT 
      unique_items.SKU, 
      unique_items.Status,
      unique_items.Defect,
      inventory.Maker,
      inventory.MajorType,
      inventory.MinorType,
      inventory.ItemName,
      inventory.ItemImagePath
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
  output$unique_items_table <- renderDT({
    # Define column mapping for user-friendly display
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图片",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Status = "库存状态",
      Defect = "物品状态"
    )
    
    # Render table with images
    render_table_with_images(
      data = unique_items_data(),     # Use the reactive data source
      column_mapping = column_mapping, # Map columns to user-friendly names
      image_column = "ItemImagePath"   # Specify the column for images
    ) %>%
      formatStyle(
        "物品状态",  # 指定要设置样式的列
        backgroundColor = styleEqual(
          c("无瑕", "瑕疵", "修复"),  # 状态值
          c("green", "red", "orange")  # 对应的背景色
        ),
        color = styleEqual(
          c("无瑕", "瑕疵", "修复"),  # 状态值
          c("white", "white", "white")  # 对应的字体颜色
        )
      )
  })
  
  
  ### SKU 模块
  
  # Automatically generate SKU when relevant inputs change
  observeEvent({
    input[["type_module-new_major_type"]]
    input[["type_module-new_minor_type"]]
    input$new_name
    input$new_maker
  }, {
    req(input[["type_module-new_major_type"]], input[["type_module-new_minor_type"]], input$new_name, input$new_maker)
    
    # Dynamically generate SKU
    sku <- generate_sku(
      item_type_data = item_type_data(),
      major_type = input[["type_module-new_major_type"]],
      minor_type = input[["type_module-new_minor_type"]],
      item_name = input$new_name,
      maker = input$new_maker
    )
    
    # Update the SKU input field
    updateTextInput(session, "new_sku", value = sku)
  })
  
  
  ### Barcode PDF 模块
  session$onFlushed(function() {
    shinyjs::disable("download_single_pdf")
    shinyjs::disable("download_batch_pdf")
  })
  

  # 单个 SKU 条形码生成逻辑
  observeEvent(input$export_single_btn, {
    if (is.null(input$new_sku) || input$new_sku == "") {
      showNotification("没有SKU生成！", type = "error")
      return()
    }
    
    # 如果勾选了重复条形码，检查数量是否为空或为 0
    if (input$repeat_barcode) {
      if (is.null(input$new_quantity) || input$new_quantity <= 0) {
        showNotification("数量不能为空且必须大于 0!", type = "error")
        return()
      }
    }
    
    # 判断是否需要重复打印
    quantity <- if (input$repeat_barcode) input$new_quantity else 1
    
    # 重复 SKU 以匹配数量
    skus <- rep(input$new_sku, quantity)
    
    # 调用生成条形码 PDF 的函数
    pdf_file <- export_barcode_pdf(
      sku = skus, 
      page_width, page_height, # 全局变量
      unit = size_unit
    )
    single_pdf_file_path(pdf_file)  # 保存生成的 PDF 路径
    
    showNotification("条形码已生成为 PDF!")
    shinyjs::enable("download_single_pdf")
  })
  
  # 批量 SKU 条形码生成逻辑
  observeEvent(input$export_batch_btn, {
    items <- added_items()
    
    if (nrow(added_items()) == 0) {
      showNotification("没有添加商品！", type = "error")
      return()
    }
    
    # 判断是否需要重复打印
    if (input$repeat_barcode) {
      skus <- unlist(mapply(function(sku, qty) rep(sku, qty), items$SKU, items$Quantity, SIMPLIFY = FALSE))
    } else {
      skus <- items$SKU
    }
    
    # 调用生成条形码 PDF 的函数
    pdf_file <- export_barcode_pdf(
      sku = skus, 
      page_width = page_width,  # 全局变量
      page_height = page_height, 
      unit = size_unit
    )
    batch_pdf_file_path(pdf_file)  # 保存生成的 PDF 路径
    
    showNotification("条形码已生成为 PDF!")
    shinyjs::enable("download_batch_pdf")
    barcode_generated(FALSE) #每新生成条形码PDF，就标记未下载
  })
  
  # 单张条形码下载逻辑
  output$download_single_pdf <- downloadHandler(
    filename = function() {
      basename(single_pdf_file_path())
    },
    content = function(file) {
      file.copy(single_pdf_file_path(), file, overwrite = TRUE)
      single_pdf_file_path(NULL)
      shinyjs::disable("download_single_pdf")
    }
  )
  
  # 批量条形码下载逻辑
  output$download_batch_pdf <- downloadHandler(
    filename = function() {
      basename(batch_pdf_file_path())
    },
    content = function(file) {
      file.copy(batch_pdf_file_path(), file, overwrite = TRUE)
      batch_pdf_file_path(NULL)
      shinyjs::disable("download_batch_pdf")
      barcode_generated(TRUE) #下载完成标记
    }
  )
  
  
  observeEvent(input$reset_btn, {
    tryCatch({
      # 清空输入控件
      updateSelectizeInput(session, "new_maker", selected = NULL)  # 清空供应商选择
      updateSelectInput(session, "new_major_type", selected = NULL)  # 清空大类选择
      updateSelectInput(session, "new_minor_type", selected = NULL)  # 清空小类选择
      updateTextInput(session, "new_name", value = "")  # 清空商品名
      updateNumericInput(session, "new_quantity", value = 0)  # 恢复数量默认值
      updateNumericInput(session, "new_product_cost", value = 0)  # 恢复成本默认值
      updateNumericInput(session, "new_shipping_cost", value = 0)  # 恢复运费默认值
      updateTextInput(session, "new_sku", value = "")  # 清空 SKU
      
      # 清空已添加的商品
      added_items(create_empty_inventory())
      
      # 重置文件输入框
      shinyjs::reset("new_item_image")
      uploaded_file(NULL)
      
      # 通知用户
      showNotification("输入已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification("清空输入时发生错误，请重试！", type = "error")
    })
  })
  
  
  #### 出库模块

  # 监听条形码输入框
  observeEvent(input$outbound_sku, {
    sku <- stri_replace_all_regex(input$outbound_sku, "\\s", "")
    
    # 如果输入为空，直接返回
    if (is.null(sku) || sku == "") {
      return()
    }
    
    # 查询数据库中是否有匹配的 SKU
    all_sku_items <- dbGetQuery(con, "
      SELECT UniqueID, Status 
      FROM unique_items 
      WHERE SKU = ?", 
                                params = list(sku)
    )
    
    # 检查是否找到 SKU
    if (nrow(all_sku_items) == 0) {
      showNotification("未找到该条形码对应的物品，请检查输入！", type = "error")
      return()
    }
    
    # 查询符合出库条件的物品
    matched_items <- all_sku_items[all_sku_items$Status == "国内入库", ]
    
    # 检查是否有符合条件的物品
    if (nrow(matched_items) == 0) {
      showNotification("该条形码的物品没有可以出库的库存！", type = "error")
      return()
    }
    
    # 获取匹配的 UniqueID
    unique_id <- matched_items$UniqueID[1]
    
    # 更新状态为 "国内出库" 并设置出库时间
    tryCatch({
      update_status(con, unique_id, "国内出库")
      
      # 记录到撤回队列
      undo_list <- undo_queue()
      undo_list <- append(undo_list, list(list(unique_id = unique_id, timestamp = Sys.time())))
      undo_queue(undo_list)  # 更新队列
      
      # 刷新 inventory 数据
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
      
      showNotification("物品出库成功！", type = "message")
      
      # 刷新物品状态追踪表
      refresh_trigger(!refresh_trigger())
      
      # 清空输入框
      updateTextInput(session, "outbound_sku", value = "")
    }, error = function(e) {
      showNotification(paste("出库操作失败:", e$message), type = "error")
    })
  })
  
  # 撤回最近出库操作
  observeEvent(input$undo_outbound_btn, {
    undo_list <- undo_queue()
    
    # 检查是否有可撤回的记录
    if (length(undo_list) == 0) {
      showNotification("没有可撤回的操作！", type = "error")
      return()
    }
    
    # 获取最近一条记录
    last_action <- tail(undo_list, 1)[[1]]
    unique_id <- last_action$unique_id
    
    # 从队列中移除最后一条记录
    undo_list <- head(undo_list, -1)
    undo_queue(undo_list)
    
    # 撤回数据库操作
    tryCatch({
      dbExecute(con, "
        UPDATE unique_items 
        SET Status = '国内入库', DomesticExitTime = NULL 
        WHERE UniqueID = ?",
                params = list(unique_id)
      )
      showNotification("撤回成功，物品状态已恢复为“国内入库”！", type = "message")
      
      # 刷新物品状态追踪表
      refresh_trigger(!refresh_trigger())
    }, error = function(e) {
      showNotification(paste("撤回操作失败:", e$message), type = "error")
    })
  })
  
  
  
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}