# Define server logic
server <- function(input, output, session) {
  # Database
  con <- db_connection()
  
  # ReactiveVal 存储 item_type_data 数据
  item_type_data <- reactiveVal()
  
  # ReactiveVal 用于存储 inventory 数据
  inventory <- reactiveVal()
  
  # ReactiveVal 用于存储 unique item 数据
  unique_item_for_report <- reactiveVal()
  
  # 用于保存用户上传的文件信息
  uploaded_file <- reactiveVal(NULL)
  
  # 声明一个 reactiveVal 用于触发unique_items_data刷新
  unique_items_data_refresh_trigger <- reactiveVal(FALSE)
  
  # 用于存储 PDF 文件路径
  single_pdf_file_path <- reactiveVal(NULL)
  batch_pdf_file_path <- reactiveVal(NULL)
  
  # 存储条形码是否已生成的状态
  barcode_generated <- reactiveVal(FALSE)  # 初始化为 FALSE
  
  ####################################################################################################################################
  
  # 应用启动时加载数据: item_type_data
  observe({
    tryCatch({
      item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
    }, error = function(e) {
      item_type_data(NULL)  # 如果出错，设为空值
      showNotification("Initiation: Failed to load item type data.", type = "error")
    })
  })
  
  # 应用启动时加载数据: inventory
  observe({
    tryCatch({
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))  # 存储到 reactiveVal
    }, error = function(e) {
      inventory(NULL)  # 如果失败，设为空
      showNotification("Initiation: Failed to load inventory data.", type = "error")
    })
  })
  
  # 切换显示/隐藏
  observeEvent(input$toggle_inventory_table, {
    shinyjs::toggle("inventory_table_container")  
  })
  
  observeEvent(input$toggle_item_table_inbound, {
    shinyjs::toggle("item_table_container_inbound")
  })
  
  observeEvent(input$toggle_item_table_defect, {
    shinyjs::toggle("item_table_container_defect")
  })
  

  ####################################################################################################################################
  ###################################################                              ###################################################
  ###################################################             渲染             ###################################################
  ###################################################                              ###################################################
  ####################################################################################################################################
  
  
  # 采购商品添加表（临时）
  added_items <- reactiveVal(create_empty_inventory() %>% select(-ShippingCost))
  
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
  
  ####################################################################################################################################
  
  # 库存表 （过滤）
  filtered_inventory <- reactive({
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
  
  ####################################################################################################################################
  
  # 物品追踪表
  unique_items_data <- reactive({
    # 当 refresh_trigger 改变时触发更新
    unique_items_data_refresh_trigger()
    
    dbGetQuery(con, "
    SELECT 
      unique_items.UniqueID, 
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
  
  # 渲染物品追踪数据表
  unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound", data = unique_items_data)
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect", data = unique_items_data)
  unique_items_table_outbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_outbound", data = unique_items_data)
  unique_items_table_sold_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_sold", data = unique_items_data)
  
  ####################################################################################################################################
  
  # 显示总采购开销（含运费）
  output$total_cost <- renderText({
    total <- sum(added_items()$Quantity * added_items()$ProductCost) + input$new_shipping_cost
    paste0("请核实本次采购总金额: ¥", format(total, big.mark = ",", scientific = FALSE),
           "（其中包含运费: ¥", input$new_shipping_cost, ")")
  })
  
  
  ####################################################################################################################################
  ###################################################                              ###################################################
  ###################################################             监听             ###################################################
  ###################################################                              ###################################################
  ####################################################################################################################################
  
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
  
  
  
  # 处理上传新图片
  observeEvent(input$new_item_image, {
    if (!is.null(input$new_item_image)) {
      # 记录新上传的文件
      uploaded_file(input$new_item_image)
      showNotification("文件已上传并记录！", type = "message")
    }
  })

  # 清空输入
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
      showNotification("请确保SKU正常显示！", type = "error")
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
          unique_image_name <- paste0(input$new_sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
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
          unique_image_name <- paste0(input$new_sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
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
  
  # Handle add item button click
  observeEvent(input$update_image_btn, {
    # 验证输入
    if (is.null(input$new_sku) || input$new_sku == "") {
      showNotification("请确保SKU正常显示！", type = "error")
      return()
    }
    
    # 检查SKU是否存在于库存表里
    existing_inventory_items <- inventory()
    existing_inventory_skus <- existing_inventory_items$SKU
    
    if (input$new_sku %in% existing_inventory_skus) {
      # 如果上传了图片，更新图片路径
      if (!is.null(uploaded_file())) {
        tryCatch({
          file_data <- uploaded_file()
          # 为图片生成唯一文件名
          unique_image_name <- paste0(input$new_sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
          output_dir <- "/var/www/images"
          final_image_path <- file.path(output_dir, unique_image_name)
          
          # 保存压缩后的图片
          save_compressed_image(
            file_path = file_data$datapath,
            output_dir = output_dir,
            image_name = unique_image_name
          )
          showNotification(paste0(input$new_sku, "图片已成功更新并保存！"), type = "message")

          # 更新库存数据
          dbExecute(con, "UPDATE inventory 
                      SET ItemImagePath = ? 
                      WHERE SKU = ?",
                    params = list(final_image_path, input$new_sku))
          
          # 更新inventory()，触发filtered_inventory_table更新
          inventory(dbGetQuery(con, "SELECT * FROM inventory"))
          
          # 触发更新刷新unique_items_data
          unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger()) 
          
        }, error = function(e) {
          showNotification("图片更新失败！", type = "error")
        })
      } else {
        showNotification("请先上传图片！", type = "error")
      } 
      # SKU 不存在，添加新记录
    } else {
      showNotification("库存中无此SKU商品，无法更新图片！", type = "error")
    }

    # 重置文件输入框
    shinyjs::reset("new_item_image")
    uploaded_file(NULL)
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
      
      # 更新 SKU 输入框(生成库存图表用)
      updateTextInput(session, "sku_inventory", value = selected_data$SKU)
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
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost)
    }
  })
  
  # Confirm button: Update database and handle images
  observeEvent(input$confirm_btn, {
    tryCatch({
      
      if (nrow(added_items()) == 0) {
        showNotification("请先录入至少一个商品!", type = "error")
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
            unique_image_name <- paste0(sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
            final_image_path <- file.path("/var/www/images", unique_image_name)
            
            tryCatch({
              # 保存新图片
              file.rename(new_image_path, final_image_path)
              new_image_path <- final_image_path
            }, error = function(e) {
              showNotification(paste("商品新图片保存失败! SKU:", sku), type = "error")
              new_image_path <- existing_item$ItemImagePath  # 回退为原始路径
            })
          } else {
            # 未上传新图片，保留现有图片路径
            new_image_path <- existing_item$ItemImagePath
          }
          
          # 更新库存数据
          dbExecute(con, "UPDATE inventory 
                      SET Quantity = ?, ProductCost = ?, ShippingCost = ?, ItemImagePath = ? 
                      WHERE SKU = ?",
                    params = list(new_quantity, round(new_ave_product_cost, 2), round(new_ave_shipping_cost, 2), new_image_path, sku))
          
          showNotification(paste("商品更新成功! SKU:", sku, ", 商品名:", item_name), type = "message")
        } else {
          # 如果 SKU 不存在，插入新商品
          unique_image_name <- if (!is.na(new_image_path) && new_image_path != "") {
            paste0(sku, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
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
                      (SKU, Maker, MajorType, MinorType, ItemName, Quantity, ProductCost, ShippingCost, ItemImagePath) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    params = list(sku, maker, major_type, minor_type, item_name, quantity, 
                                  round(product_cost, 2), round(unit_shipping_cost, 2), new_image_path))

          showNotification(paste("新商品创建成功! SKU:", sku, ", 商品名:", item_name), type = "message")
        }
      }
      
      # 更新inventory()，触发filtered_inventory_table更新
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
      
      # 触发更新刷新unique_items_data
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger()) 
      
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
          "采购",
          "未知",
          format(Sys.time(), "%Y-%m-%d", tz = "Asia/Shanghai")
        )))
      }))
      
      # Validate data
      if (is.null(batch_data) || nrow(batch_data) == 0) {
        showNotification("采购数据无效，请检查输入！", type = "error")
        return()
      }
      
      # Convert to data frame
      batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)

      # Insert into database
      dbBegin(con)
      tryCatch({
        # Insert rows one by one
        for (i in 1:nrow(batch_data)) {
          # Ensure the parameters are passed as an unnamed vector
          dbExecute(con, "INSERT INTO unique_items (UniqueID, SKU, ProductCost, DomesticShippingCost, Status, Defect, PurchaseTime) VALUES (?, ?, ?, ?, ?, ?, ?)",
                    unname(as.vector(batch_data[i, ]))  # Use `unname` to avoid named parameters
          )
        }
        dbCommit(con)  # Commit transaction
        showNotification("所有采购货物已成功登记！", type = "message")
       
        unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())  #触发更新
        
      }, error = function(e) {
        dbRollback(con)  # Rollback on error
        showNotification(paste("采购登记失败:", e$message), type = "error")
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
  

  ################################################################
  ##                                                            ##
  ## SKU BARCODE模块                                            ##
  ##                                                            ##
  ################################################################
  
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
  

  ################################################################
  ##                                                            ##
  ## 入库模块                                                   ##
  ##                                                            ##
  ################################################################
  
  # 监听 SKU 输入
  observeEvent(input$inbound_sku, {
    handleSkuInput(
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
  })
  
  # 确认入库逻辑
  observeEvent(input$confirm_inbound_btn, {
    handleOperation(
      operation_name = "入库",
      sku_input = input$inbound_sku,
      output_name = "inbound_item_info",
      query_status = "采购",
      update_status_value = "国内入库",
      count_label = "待入库数",
      count_field = "PendingQuantity",
      con = con,
      output = output,
      refresh_trigger = unique_items_data_refresh_trigger,
      session = session,
      input = input # 传递 input 对象
    )
  })
  
  # 监听选中行并更新 SKU
  observeEvent(unique_items_table_inbound_selected_row(), {
    if (!is.null(unique_items_table_inbound_selected_row()) && length(unique_items_table_inbound_selected_row()) > 0) {
      selected_sku <- unique_items_data()[unique_items_table_inbound_selected_row(), "SKU", drop = TRUE]
      updateTextInput(session, "inbound_sku", value = selected_sku)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 瑕疵商品模块                                               ##
  ##                                                            ##
  ################################################################
  
  observeEvent(input$defect_register, {
    sku <- trimws(input$defect_sku)  # 清除空格
    quantity <- input$defect_quantity
    
    # 校验输入
    if (is.null(sku) || sku == "" || is.null(quantity) || quantity <= 0) {
      showNotification("请输入有效的条形码和数量！", type = "error")
      return()
    }
    
    # 查询 SKU 数据
    sku_data <- dbGetQuery(con, "SELECT * FROM unique_items WHERE SKU = ?", params = list(sku))
    
    if (nrow(sku_data) == 0) {
      showNotification("条形码未找到！", type = "error")
      return()
    }
    
    # 检查符合条件的无瑕商品数量 （可修改的物品必须是国内入库）
    available_items <- sku_data[sku_data$Status == "国内入库" & sku_data$Defect == "无瑕", ]
    available_quantity <- nrow(available_items)
    
    if (quantity > available_quantity) {
      showNotification("库存不足，无法登记为瑕疵品！", type = "error")
      return()
    }
    
    # 更新选定数量为“瑕疵”
    tryCatch({
      selected_ids <- available_items$UniqueID[1:quantity]  # 获取所需数量的 UniqueID
      lapply(selected_ids, function(unique_id) {
        update_status(con, unique_id, "国内入库", defect_status = "瑕疵",
                      refresh_trigger = unique_items_data_refresh_trigger)
      })
      showNotification("瑕疵品登记成功！", type = "message")
    }, error = function(e) {
      showNotification(paste("登记失败：", e$message), type = "error")
    })
  })
  
  observeEvent(input$repair_register, {
    sku <- trimws(input$repair_sku)  # 清除空格
    quantity <- input$repair_quantity
    
    # 校验输入
    if (is.null(sku) || sku == "" || is.null(quantity) || quantity <= 0) {
      showNotification("请输入有效的条形码和数量！", type = "error")
      return()
    }
    
    # 查询 SKU 数据
    sku_data <- dbGetQuery(con, "SELECT * FROM unique_items WHERE SKU = ?", params = list(sku))
    
    if (nrow(sku_data) == 0) {
      showNotification("条形码未找到！", type = "error")
      return()
    }
    
    # 检查瑕疵品数量 （可修改的物品必须是国内入库）
    defect_items <- sku_data[sku_data$Status == "国内入库" & sku_data$Defect == "瑕疵", ]
    defect_quantity <- nrow(defect_items)
    
    if (quantity > defect_quantity) {
      showNotification("瑕疵品数量不足，无法修复！", type = "error")
      return()
    }
    
    # 更新选定数量为“修复”
    tryCatch({
      selected_ids <- defect_items$UniqueID[1:quantity]  # 获取所需数量的 UniqueID
      lapply(selected_ids, function(unique_id) {
        update_status(con, unique_id, "国内入库", defect_status = "修复",
                      refresh_trigger = unique_items_data_refresh_trigger)
      })
      showNotification("瑕疵品修复登记成功！", type = "message")
    }, error = function(e) {
      showNotification(paste("修复登记失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 出库商品模块                                               ##
  ##                                                            ##
  ################################################################
  
  # 监听出库 SKU 输入
  observeEvent(input$outbound_sku, {
    handleSkuInput(
      sku_input = input$outbound_sku,
      output_name = "outbound_item_info",
      count_label = "可出库数",
      count_field = "AvailableForOutbound",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
  })
  
  # 确认出库逻辑
  observeEvent(input$confirm_outbound_btn, {
    handleOperation(
      operation_name = "出库",
      sku_input = input$outbound_sku,
      output_name = "outbound_item_info",
      query_status = "国内入库",
      update_status_value = "国内出库",
      count_label = "可出库数",
      count_field = "AvailableForOutbound",
      con = con,
      output = output,
      refresh_trigger = unique_items_data_refresh_trigger,
      session = session
    )
  })
  
  # 监听选中行并更新出库 SKU
  observeEvent(unique_items_table_outbound_selected_row(), {
    if (!is.null(unique_items_table_outbound_selected_row()) && length(unique_items_table_outbound_selected_row()) > 0) {
      selected_sku <- unique_items_data()[unique_items_table_outbound_selected_row(), "SKU", drop = TRUE]
      updateTextInput(session, "outbound_sku", value = selected_sku)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 售出商品模块                                               ##
  ##                                                            ##
  ################################################################
  
  # 监听售出 SKU 输入
  observeEvent(input$sold_sku, {
    handleSkuInput(
      sku_input = input$sold_sku,
      output_name = "sold_item_info",
      count_label = "可售出数",
      count_field = "AvailableForSold",
      con = con,
      output = output,
      placeholder_path = placeholder_300px_path,
      host_url = host_url
    )
  })
  
  # 确认售出逻辑
  observeEvent(input$confirm_sold_btn, {
    handleOperation(
      operation_name = "售出",
      sku_input = input$sold_sku,
      output_name = "sold_item_info",
      query_status = "国内入库",
      update_status_value = "国内售出",
      count_label = "可售出数",
      count_field = "AvailableForSold",
      con = con,
      output = output,
      refresh_trigger = unique_items_data_refresh_trigger,
      session = session
    )
  })
  
  # 监听选中行并更新售出 SKU
  observeEvent(unique_items_table_sold_selected_row(), {
    if (!is.null(unique_items_table_sold_selected_row()) && length(unique_items_table_sold_selected_row()) > 0) {
      selected_sku <- unique_items_data()[unique_items_table_sold_selected_row(), "SKU", drop = TRUE]
      updateTextInput(session, "sold_sku", value = selected_sku)
    }
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 报表模块                                                   ##
  ##                                                            ##
  ################################################################
  
  # 查询按钮点击事件
  observeEvent(input$search_report_btn, {
    sku <- trimws(input$query_sku)
    
    # 校验输入
    if (is.null(sku) || sku == "") {
      showNotification("请输入有效的 SKU！", type = "error")
      output$query_item_info <- renderUI({
        div(
          tags$img(src = placeholder_path, height = "300px", style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 20px;"),
          tags$p("未找到相关商品信息。", style = "color: red; font-size: 16px;")
        )
      })
      output$inventory_status_chart <- renderPlot({ NULL })
      output$defect_status_chart <- renderPlot({ NULL })
      return()
    }
    
    # 查询商品详情
    sku_query <- "
        SELECT 
          ItemName, Maker, MajorType, MinorType, Quantity, 
          ProductCost, ShippingCost, ItemImagePath 
        FROM inventory
        WHERE SKU = ?"
    sku_data <- dbGetQuery(con, sku_query, params = list(sku))
    
    if (nrow(sku_data) == 0) {
      showNotification("未找到该 SKU 对应的商品信息！", type = "error")
      output$query_item_info <- renderUI({
        div(
          tags$img(src = placeholder_path, height = "300px", style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 20px;"),
          tags$p("未找到相关商品信息。", style = "color: red; font-size: 16px;")
        )
      })
      output$inventory_status_chart <- renderPlot({ NULL })
      output$defect_status_chart <- renderPlot({ NULL })
      return()
    }
    
    # 渲染商品信息
    output$query_item_info <- renderUI({
      img_path <- ifelse(
        is.na(sku_data$ItemImagePath[1]),
        placeholder_path,
        paste0(host_url, "/images/", basename(sku_data$ItemImagePath[1]))
      )
      
      div(
        style = "display: flex; align-items: center; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); padding: 20px; background-color: #f9f9f9;",
        # 图片区域
        div(
          style = "flex: 1; text-align: center; margin-right: 20px;",
          tags$img(
            src = img_path, 
            height = "300px", 
            style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);"
          )
        ),
        # 文字信息区域
        div(
          style = "flex: 2; display: flex; flex-direction: column; justify-content: center;",
          tags$p(tags$b("商品名称："), style = "font-size: 18px; color: #333;", sku_data$ItemName[1]),
          tags$p(tags$b("供应商："), style = "font-size: 18px; color: #333;", sku_data$Maker[1]),
          tags$p(tags$b("分类："), style = "font-size: 18px; color: #333;", paste(sku_data$MajorType[1], "/", sku_data$MinorType[1])),
          tags$p(tags$b("库存数量："), style = "font-size: 18px; color: #333;", sku_data$Quantity[1]),
          tags$p(tags$b("平均成本："), style = "font-size: 18px; color: #333;", sprintf("¥%.2f", sku_data$ProductCost[1])),
          tags$p(tags$b("平均运费："), style = "font-size: 18px; color: #333;", sprintf("¥%.2f", sku_data$ShippingCost[1]))
        )
      )
    })
    
    output$inventory_status_chart <- renderPlotly({
      inventory_status_query <- "
    SELECT Status, COUNT(*) AS Count
    FROM unique_items
    WHERE SKU = ?
    GROUP BY Status"
      inventory_status_data <- dbGetQuery(con, inventory_status_query, params = list(input$query_sku))
      
      # 明确类别顺序
      status_levels <- c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货")
      inventory_status_data$Status <- factor(inventory_status_data$Status, levels = status_levels)
      
      if (nrow(inventory_status_data) == 0) {
        # 无数据时渲染默认饼图
        plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+percent")
      } else {
        # 渲染库存状态饼图
        plot_ly(
          data = inventory_status_data,
          labels = ~Status,
          values = ~Count,
          type = "pie",
          textinfo = "label+percent",
          insidetextorientation = "radial",
          marker = list(
            colors = c("lightgray", "#c7e89b", "black", "#46a80d", "#173b02", "darkgray", "red")
          )
        ) %>%
          layout(
            title = "库存状态分布",
            showlegend = TRUE
          )
      }
    })
    
  })

  
  ################################################################
  ##                                                            ##
  ## 移库模块（管理员模式）                                     ##
  ##                                                            ##
  ################################################################
  
  # # 监听移库按钮点击
  # observeEvent(input$move_selected, {
  #   # 获取选中的行索引
  #   selected_row <- input$unique_items_table_rows_selected
  #   
  #   # 检查是否有选中行
  #   if (is.null(selected_row) || length(selected_row) == 0) {
  #     showNotification("请先在物品状态表中选择一行物品再执行移库操作！", type = "error")
  #     return()
  #   }
  #   
  #   # 检查是否选择了移库目标
  #   target_status <- input$target_status
  #   if (is.null(target_status) || target_status == "") {
  #     showNotification("请选择一个移库目标！", type = "error")
  #     return()
  #   }
  #   
  #   # 获取选中行的数据
  #   selected_data <- unique_items_data()[selected_row, ]
  #   unique_id <- selected_data$UniqueID
  #   current_status <- selected_data$Status
  #   
  #   # 如果当前状态已经是目标状态
  #   if (current_status == target_status) {
  #     showNotification("物品已经在目标状态，无需移库！", type = "message")
  #     return()
  #   }
  #   
  #   # 执行移库操作
  #   tryCatch({
  #     update_status(con, unique_id, target_status)
  #     showNotification(paste("物品成功移至状态：", target_status), type = "message")
  # 
  #   }, error = function(e) {
  #     showNotification(paste("移库操作失败：", e$message), type = "error")
  #   })
  # })
  

  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}