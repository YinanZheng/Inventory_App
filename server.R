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
  undo_outbound_queue <- reactiveVal(list())  # 出库撤回队列
  undo_sold_queue <- reactiveVal(list()) # 售出撤回队列

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
      
      # 更新 SKU 输入框(生成库存图表用)
      updateTextInput(session, "sku_inventory", value = selected_data$SKU)
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
    paste0("请核实本次采购总金额: ¥", format(total, big.mark = ",", scientific = FALSE),
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
      
      # 刷新库存数据
      inventory({dbGetQuery(con, "SELECT * FROM inventory")})
      
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
        # 切换触发器值，确保刷新
        refresh_trigger(!refresh_trigger()) 
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
  
  
  ####################################################################
  
  
  observeEvent(input$inbound_sku, {
    sku <- stri_trim_both(input$inbound_sku) # 清理空格
    
    if (is.null(sku) || sku == "") {
      output$inbound_item_info <- renderUI(NULL)
      return()
    }
    
    # 查询 SKU 数据
    item_info <- dbGetQuery(con, "
    SELECT 
      ItemImagePath, ItemName, Maker, MajorType, MinorType, 
      COUNT(*) as PendingQuantity
    FROM 
      unique_items
    JOIN 
      inventory 
    ON 
      unique_items.SKU = inventory.SKU
    WHERE 
      unique_items.SKU = ? AND unique_items.Status = '采购'
    GROUP BY 
      ItemImagePath, ItemName, Maker, MajorType, MinorType
  ", params = list(sku))
    
    # 检查是否有结果
    if (nrow(item_info) == 0) {
      showNotification("未找到该条形码对应的物品或无待入库数量！", type = "error")
      output$inbound_item_info <- renderUI(NULL)
      return()
    }
    
    img_path <- paste0(host_url, "/images/", basename(item_info$ItemImagePath[1]))
    
    # 渲染物品信息
    output$inbound_item_info <- renderUI({
      fluidRow(
        column(
          4, 
          div(
            style = "text-align: center; padding: 15px;",
            img(
              src = img_path, 
              height = "300px", 
              style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);"
            ),
          )
        ),
        column(
          8,
          div(
            style = "padding: 20px; background-color: #f7f7f7; border: 1px solid #e0e0e0; border-radius: 8px; 
                box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); height: 100%;",
            tags$h4(
              "商品信息", 
              style = "border-bottom: 3px solid #4CAF50; margin-bottom: 15px; padding-bottom: 8px; font-weight: bold; color: #333;"
            ),
            tags$table(
              style = "width: 100%; font-size: 16px; color: #444;",
              tags$tr(
                tags$td(tags$strong("商品名:"), style = "padding: 8px 10px; width: 120px; vertical-align: top;"),
                tags$td(tags$span(item_info$ItemName[1], style = "color: #4CAF50; font-weight: bold;"))
              ),
              tags$tr(
                tags$td(tags$strong("供应商:"), style = "padding: 8px 10px; vertical-align: top;"),
                tags$td(tags$span(item_info$Maker[1], style = "color: #4CAF50;"))
              ),
              tags$tr(
                tags$td(tags$strong("大类:"), style = "padding: 8px 10px; vertical-align: top;"),
                tags$td(tags$span(item_info$MajorType[1], style = "color: #4CAF50;"))
              ),
              tags$tr(
                tags$td(tags$strong("小类:"), style = "padding: 8px 10px; vertical-align: top;"),
                tags$td(tags$span(item_info$MinorType[1], style = "color: #4CAF50;"))
              ),
              tags$tr(
                tags$td(tags$strong("待入库数:"), style = "padding: 8px 10px; vertical-align: top;"),
                tags$td(tags$span(item_info$PendingQuantity[1], style = "color: #4CAF50; font-weight: bold;"))
              )
            )
          )
        )
      )
    })
    
  })
  
  
  
  ## 物品追踪表
  unique_items_data <- reactive({
    # 当 refresh_trigger 改变时触发更新
    refresh_trigger()
    
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
  
  # 渲染 unique_items 数据表
  output$unique_items_table <- renderDT({
    refresh_trigger()  # 触发器变化时重新加载数据
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
          c("未知", "无瑕", "瑕疵", "修复"),  # 状态值
          c("darkgray","green", "red", "orange")  # 对应的背景色
        ),
        color = styleEqual(
          c("未知", "无瑕", "瑕疵", "修复"),  # 状态值
          c("black","white", "white", "white")  # 对应的字体颜色
        )
      ) %>%
      # 设置库存状态列的样式
      formatStyle(
        "库存状态",  # 指定列名
        backgroundColor = styleEqual(
          c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货"),  # 状态值 
          c("lightblue", "blue", "purple", "darkblue", "yellow", "brown", "red")  # 对应的背景色
        ),
        color = styleEqual(
          c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货"),  # 状态值
          c("black", "black", "white", "white", "black", "white", "white")  # 对应的文字颜色
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
    
    # 检查符合条件的无瑕商品数量
    available_items <- sku_data[sku_data$Status == "国内入库" & sku_data$Defect == "无瑕", ]
    available_quantity <- nrow(available_items)
    
    if (quantity > available_quantity) {
      showNotification("库存不足，无法登记为瑕疵品！", type = "error")
      return()
    }
    
    # 更新选定数量为“瑕疵”
    tryCatch({
      for (i in 1:quantity) {
        unique_id <- available_items$UniqueID[i]
        update_status(con, unique_id, "瑕疵")
      }
      showNotification("瑕疵品登记成功！", type = "message")
      refresh_trigger(!refresh_trigger())  # 刷新数据表
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
    
    # 检查瑕疵数量
    defect_quantity <- nrow(sku_data[sku_data$Defect == "瑕疵", ])
    if (quantity > defect_quantity) {
      showNotification("瑕疵品数量不足，无法修复！", type = "error")
      return()
    }
    
    # 更新选定数量为“修复”
    tryCatch({
      for (i in 1:quantity) {
        unique_id <- sku_data$UniqueID[sku_data$Defect == "瑕疵"][i]
        update_status(con, unique_id, "修复")
      }
      showNotification("瑕疵品修复登记成功！", type = "message")
      refresh_trigger(!refresh_trigger())  # 刷新数据表
    }, error = function(e) {
      showNotification(paste("修复登记失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 移库模块                                                   ##
  ##                                                            ##
  ################################################################
  
  # 国内出库逻辑
  handleSKU(
    con = con,
    input = input,
    session = session,
    sku_input_id = "outbound_sku",
    target_status = "国内出库",
    valid_current_status = c("国内入库"),  # 仅限国内入库状态
    undo_queue = undo_outbound_queue,
    refresh_trigger = refresh_trigger,
    inventory = inventory,
    notification_success = "物品出库成功！",
    notification_error = "出库操作失败：",
    record_undo = TRUE
  )
  
  undoLastAction(
    con = con, input = input,
    undo_btn = "undo_outbound_btn",
    undo_queue = undo_outbound_queue,
    refresh_trigger = refresh_trigger,
    inventory = inventory,
    notification_success = "撤回成功，物品状态已恢复！",
    notification_error = "撤回操作失败："
  )
  
  # 国内售出逻辑
  handleSKU(
    input = input,
    session = session,
    sku_input_id = "sold_sku",
    target_status = "国内售出",
    valid_current_status = c("国内入库"),  # 仅限国内入库状态
    undo_queue = undo_sold_queue,
    con = con,
    refresh_trigger = refresh_trigger,
    inventory = inventory,
    notification_success = "物品售出成功！",
    notification_error = "售出操作失败：",
    record_undo = TRUE
  )
  
  undoLastAction(
    con = con, input = input,
    undo_btn = "undo_sold_btn",
    undo_queue = undo_sold_queue,
    refresh_trigger = refresh_trigger,
    inventory = inventory,
    notification_success = "撤回成功，物品状态已恢复！",
    notification_error = "撤回操作失败："
  )
  
  # 监听移库按钮点击
  observeEvent(input$move_selected, {
    # 获取选中的行索引
    selected_row <- input$unique_items_table_rows_selected
    
    # 检查是否有选中行
    if (is.null(selected_row) || length(selected_row) == 0) {
      showNotification("请先在物品状态表中选择一行物品再执行移库操作！", type = "error")
      return()
    }
    
    # 检查是否选择了移库目标
    target_status <- input$target_status
    if (is.null(target_status) || target_status == "") {
      showNotification("请选择一个移库目标！", type = "error")
      return()
    }
    
    # 获取选中行的数据
    selected_data <- unique_items_data()[selected_row, ]
    unique_id <- selected_data$UniqueID
    current_status <- selected_data$Status
    
    # 如果当前状态已经是目标状态
    if (current_status == target_status) {
      showNotification("物品已经在目标状态，无需移库！", type = "message")
      return()
    }
    
    # 执行移库操作
    tryCatch({
      update_status(con, unique_id, target_status)
      showNotification(paste("物品成功移至状态：", target_status), type = "message")
      
      # 刷新表格数据
      refresh_trigger(!refresh_trigger())
    }, error = function(e) {
      showNotification(paste("移库操作失败：", e$message), type = "error")
    })
  })
  
  
  
  ################################################################
  ##                                                            ##
  ## 报表模块                                                   ##
  ##                                                            ##
  ################################################################
  
  observeEvent(input$unique_items_table_rows_selected, {
    selected_row <- input$unique_items_table_rows_selected
    
    if (!is.null(selected_row)) {
      # 从 unique_items_data 中获取选中的 SKU
      selected_sku <- unique_items_data()[selected_row, "SKU"]
      
      # 更新 SKU 输入框 (生成报表用)
      updateTextInput(session, "sku_inventory", value = selected_sku)
    }
  })
  
  # 监听 SKU 输入框的变化，更新 unique_item_for_report
  observeEvent(input$sku_inventory, {
    sku <- trimws(input$sku_inventory)  # 清除空格
    
    if (is.null(sku) || sku == "") {
      # 清空数据和图表
      unique_item_for_report(NULL)
      output$inventory_overview_plot <- renderPlotly(NULL)
      return()
    }
    
    # 更新 unique_item_for_report
    unique_item_for_report(get_inventory_overview(con, sku))
  })
  
  # 监听 unique_item_for_report，更新图表
  observe({
    # 确保 unique_item_for_report 存在有效数据
    req(unique_item_for_report())
    
    inventory_data <- unique_item_for_report()
    
    # 转换为数据框
    data <- data.frame(
      组别 = rep(c("国内库存", "在途运输", "美国库存"), each = 3),
      状态 = rep(c("瑕疵", "修复", "无瑕"), 3),
      数量 = c(
        inventory_data$domestic_instock$defect %||% 0, 
        inventory_data$domestic_instock$repaired %||% 0, 
        inventory_data$domestic_instock$pristine %||% 0,
        inventory_data$in_transit$defect %||% 0, 
        inventory_data$in_transit$repaired %||% 0, 
        inventory_data$in_transit$pristine %||% 0,
        inventory_data$us_instock$defect %||% 0, 
        inventory_data$us_instock$repaired %||% 0, 
        inventory_data$us_instock$pristine %||% 0
      )
    )
    
    # 如果总数为 0，清空表格并通知
    if (sum(data$数量) == 0) {
      output$inventory_overview_table <- renderTable({
        data.frame(组别 = character(0), 状态 = character(0), 数量 = numeric(0))
      })
      showNotification("未找到有效的库存数据！", type = "error")
      return()
    }
    
    # 渲染静态表格
    output$inventory_overview_table <- renderTable({
      data
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
  })
  
  
  # Disconnect from the database on app stop
  onStop(function() {
    dbDisconnect(con)
  })
}