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
  
  # 用于存储粘贴图片数据
  pasted_file <- reactiveVal(NULL)  
  
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
  observeEvent(input$toggle_item_table_inbound, {
    shinyjs::toggle("item_table_container_purchase")
  })
  
  observeEvent(input$toggle_item_table_inbound, {
    shinyjs::toggle("item_table_container_inbound")
  })
  
  observeEvent(input$toggle_item_table_defect, {
    shinyjs::toggle("item_table_container_defect")
  })
  
  observeEvent(input$toggle_item_table_inbound, {
    shinyjs::toggle("item_table_container_outbound")
  })
  
  observeEvent(input$toggle_item_table_inbound, {
    shinyjs::toggle("item_table_container_sold")
  })
  
  observeEvent(input$toggle_inventory_table, {
    shinyjs::toggle("inventory_table_container")  
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
      selection = "multiple",
      image_column = "ItemImagePath"  # Specify the correct image column
    )
  })
  
  ####################################################################################################################################
  
  # 库存表 （过滤）
  filtered_inventory <- reactive({
    # req(input[["type_module-new_major_type"]], input[["type_module-new_minor_type"]])
    
    # Filter the inventory data
    # result <- inventory() %>%
    #   filter(MajorType == input[["type_module-new_major_type"]], MinorType == input[["type_module-new_minor_type"]]) %>%
    #   select(SKU, Maker, MajorType, MinorType, ItemName, Quantity, ProductCost, ShippingCost, ItemImagePath)  # Ensure Maker is included
    
    result <- inventory()
    
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
      unique_items.ProductCost,
      unique_items.DomesticShippingCost,
      unique_items.Status,
      unique_items.Defect,
      unique_items.PurchaseTime,
      unique_items.DomesticEntryTime,
      unique_items.DomesticExitTime,
      unique_items.DomesticSoldTime,
      unique_items.UsEntryTime,
      unique_items.UsSoldTime,
      unique_items.ReturnTime,
      unique_items.updated_at,
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
      unique_items.updated_at DESC
  ")
  })
  
  # 渲染物品追踪数据表
  unique_items_table_purchase_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_purchase",
                                                        column_mapping <- list(
                                                          SKU = "条形码",
                                                          ItemName = "商品名",
                                                          ItemImagePath = "商品图片",
                                                          Maker = "供应商",
                                                          MajorType = "大类",
                                                          MinorType = "小类",
                                                          ProductCost = "成本",
                                                          DomesticShippingCost = "平摊运费",
                                                          PurchaseTime = "采购日期",
                                                          DomesticEntryTime = "国内入库日期",
                                                          Status = "库存状态",
                                                          Defect = "物品状态"
                                                        ), data = unique_items_data)
  
  unique_items_table_inbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_inbound",
                                                        column_mapping <- list(
                                                          SKU = "条形码",
                                                          ItemName = "商品名",
                                                          ItemImagePath = "商品图片",
                                                          Maker = "供应商",
                                                          MajorType = "大类",
                                                          MinorType = "小类",
                                                          ProductCost = "成本",
                                                          DomesticShippingCost = "平摊运费",
                                                          PurchaseTime = "采购日期",
                                                          DomesticEntryTime = "国内入库日期",
                                                          Status = "库存状态",
                                                          Defect = "物品状态"
                                                        ), data = unique_items_data)
  
  unique_items_table_manage_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_manage",
                                                       column_mapping <- list(
                                                         SKU = "条形码",
                                                         ItemName = "商品名",
                                                         ItemImagePath = "商品图片",
                                                         Maker = "供应商",
                                                         MajorType = "大类",
                                                         MinorType = "小类",
                                                         ProductCost = "成本",
                                                         DomesticShippingCost = "平摊运费",
                                                         PurchaseTime = "采购日期",
                                                         DomesticEntryTime = "国内入库日期",
                                                         DomesticExitTime = "国内出库日期",
                                                         DomesticSoldTime = "国内售出日期",
                                                         Status = "库存状态",
                                                         Defect = "物品状态"
                                                       ), selection = "multiple", data = unique_items_data)
  
  unique_items_table_defect_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_defect",
                                                       column_mapping <- list(
                                                         SKU = "条形码",
                                                         ItemName = "商品名",
                                                         ItemImagePath = "商品图片",
                                                         Maker = "供应商",
                                                         MajorType = "大类",
                                                         MinorType = "小类",
                                                         ProductCost = "成本",
                                                         DomesticShippingCost = "平摊运费",
                                                         PurchaseTime = "采购日期",
                                                         DomesticEntryTime = "国内入库日期",
                                                         Status = "库存状态",
                                                         Defect = "物品状态"
                                                       ), data = unique_items_data)
  
  unique_items_table_outbound_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_outbound", 
                                                         column_mapping <- list(
                                                           SKU = "条形码",
                                                           ItemName = "商品名",
                                                           ItemImagePath = "商品图片",
                                                           Maker = "供应商",
                                                           MajorType = "大类",
                                                           MinorType = "小类",
                                                           ProductCost = "成本",
                                                           DomesticShippingCost = "平摊运费",
                                                           PurchaseTime = "采购日期",
                                                           DomesticEntryTime = "国内入库日期",
                                                           DomesticExitTime = "国内出库日期",
                                                           Status = "库存状态",
                                                           Defect = "物品状态"
                                                         ), data = unique_items_data)
  
  unique_items_table_sold_selected_row <- callModule(uniqueItemsTableServer, "unique_items_table_sold",
                                                     column_mapping <- list(
                                                       SKU = "条形码",
                                                       ItemName = "商品名",
                                                       ItemImagePath = "商品图片",
                                                       Maker = "供应商",
                                                       MajorType = "大类",
                                                       MinorType = "小类",
                                                       ProductCost = "成本",
                                                       DomesticShippingCost = "平摊运费",
                                                       PurchaseTime = "采购日期",
                                                       DomesticEntryTime = "国内入库日期",
                                                       DomesticSoldTime = "国内售出日期",
                                                       Status = "库存状态",
                                                       Defect = "物品状态"
                                                     ), data = unique_items_data)
  
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
      tryCatch({
        # 记录上传的文件
        uploaded_file(input$new_item_image)
        file_name <- input$new_item_image$name
        file_type <- input$new_item_image$type
        
        # 校验文件格式
        if (!(file_type %in% c("image/png", "image/jpeg"))) {
          uploaded_file(NULL)
          showNotification("仅支持上传 PNG 或 JPEG 图片！", type = "error")
          return()
        }
        
        showNotification(paste("文件已上传:", file_name), type = "message")
      }, error = function(e) {
        showNotification(paste("文件上传处理失败:", e$message), type = "error")
      })
    }
  })
  
  # 处理粘贴图片显示预览和基础信息
  observeEvent(input$pasted_image, {
    if (!is.null(input$pasted_image)) {
      tryCatch({
        pasted_file(input$pasted_image)  # 保存到 reactiveVal
        
        # 显示进度条
        shinyjs::show("upload_progress")
        update_progress(0)  # 初始化进度为 0%
        
        # 保存粘贴的图片到临时路径
        temp_path <- tempfile(fileext = ".jpg")
        update_progress(20)  # 更新进度为 20%
        
        base64_decode_image(input$pasted_image, temp_path)
        update_progress(40)  # 更新进度为 40%
        
        # 获取图片信息
        img <- magick::image_read(temp_path)
        update_progress(60)  # 更新进度为 60%
        
        img_info <- magick::image_info(img)
        
        # 检查文件大小（如果图片文件过大，给出警告并中止处理）
        file_size_kb <- file.size(temp_path) / 1024
        if (file_size_kb > 5000) { # 假设最大允许 5 MB
          shinyjs::hide("upload_progress")
          showNotification("图片过大，请粘贴小于 5 MB 的图片。", type = "error")
          return()
        }
        update_progress(80)  # 更新进度为 80%
        
        # 显示图片预览
        output$pasted_image_preview <- renderUI({
          div(
            tags$img(src = input$pasted_image, height = "200px", style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 10px;"),
            tags$p(
              style = "color: #007BFF; font-size: 14px;",
              paste0("已粘贴图片: ", img_info$width, "x", img_info$height, " 分辨率, 文件大小 ~", format(file_size_kb, digits = 2), " KB")
            ),
            actionButton("clear_pasted_image", "清除图片", icon = icon("trash"), class = "btn-danger", style = "margin-top: 10px;")
          )
        })
        
        update_progress(100)  # 更新进度为 100%
        
        # 隐藏默认提示文字
        shinyjs::hide("paste_prompt")
        
        # 暂存到 uploaded_file()
        uploaded_file(list(datapath = temp_path, name = "pasted_image.jpg"))
        
        # 隐藏进度条（延迟 1 秒，给用户反馈）
        shinyjs::delay(1000, shinyjs::hide("upload_progress"))
        showNotification("图片粘贴成功！", type = "message")
      }, error = function(e) {
        # 隐藏进度条并提示错误
        shinyjs::hide("upload_progress")
        showNotification(paste("图片粘贴失败:", e$message), type = "error")
      })
    }
  })
  
  # 清除粘贴图片预览并恢复提示
  observeEvent(input$clear_pasted_image, {
    uploaded_file(NULL)
    pasted_file(NULL)
    output$pasted_image_preview <- renderUI({ NULL })  # 移除图片预览
    shinyjs::show("paste_prompt")
    showNotification("已清除粘贴的图片！", type = "message")
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
    
    # 检查是否存在该 SKU 的库存记录
    inventory_item <- tryCatch({
      dbGetQuery(con, "SELECT ItemImagePath FROM inventory WHERE SKU = ?", params = list(input$new_sku))
    }, error = function(e) {
      showNotification("检查库存时发生错误！", type = "error")
      return(data.frame())
    })
    
    existing_inventory_path <- if (nrow(inventory_item) > 0) inventory_item$ItemImagePath[1] else NULL
    
    # 上传或粘贴图片处理
    new_image_path <- process_image_upload(
      sku = input$new_sku,
      file_data = uploaded_file(),
      base64_data = pasted_file(),
      inventory_path = existing_inventory_path
    )
    
    # 添加或更新记录
    existing_items <- added_items()
    existing_skus <- existing_items$SKU
    if (input$new_sku %in% existing_skus) {
      sku_index <- which(existing_skus == input$new_sku)
      current_image_path <- existing_items$ItemImagePath[sku_index]
      final_image_path <- if (!is.na(new_image_path) && new_image_path != "") {
        new_image_path
      } else {
        current_image_path
      }
      existing_items[sku_index, ] <- data.frame(
        SKU = input$new_sku,
        Maker = input$new_maker,
        MajorType = input[["type_module-new_major_type"]],
        MinorType = input[["type_module-new_minor_type"]],
        ItemName = input$new_name,
        Quantity = input$new_quantity,
        ProductCost = round(input$new_product_cost, 2),
        ItemImagePath = final_image_path,
        stringsAsFactors = FALSE
      )
      added_items(existing_items)
      showNotification(paste("SKU 已更新:", input$new_sku, "已覆盖旧记录"), type = "message")
    } else {
      # 添加新记录
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
    
    # 重置
    shinyjs::reset("new_item_image")
    uploaded_file(NULL)
    pasted_file(NULL)
    output$pasted_image_preview <- renderUI({ NULL })
    shinyjs::show("paste_prompt")
  })
  
  # Handle image update button click
  observeEvent(input$update_image_btn, {
    # 验证输入
    if (is.null(input$new_sku) || input$new_sku == "") {
      showNotification("请确保SKU正常显示！", type = "error")
      return()
    }
    
    # 检查 SKU 是否存在于库存表里
    existing_inventory_items <- inventory()
    existing_inventory_skus <- existing_inventory_items$SKU
    
    if (input$new_sku %in% existing_inventory_skus) {
      # 获取当前 SKU 对应的图片路径
      existing_item <- existing_inventory_items[existing_inventory_items$SKU == input$new_sku, ]
      existing_image_path <- existing_item$ItemImagePath[1]
      
      # 处理图片上传或粘贴
      updated_image_path <- process_image_upload(
        sku = input$new_sku,
        file_data = uploaded_file(),
        base64_data = pasted_file(),
        inventory_path = existing_image_path
      )
      
      # 检查处理结果
      if (!is.null(updated_image_path) && !is.na(updated_image_path)) {
        tryCatch({
          # 更新库存数据
          dbExecute(con, "UPDATE inventory 
                        SET ItemImagePath = ? 
                        WHERE SKU = ?",
                    params = list(updated_image_path, input$new_sku))
          
          # 更新 `inventory()`，触发 `filtered_inventory_table` 更新
          inventory(dbGetQuery(con, "SELECT * FROM inventory"))
          
          # 触发刷新 `unique_items_data`
          unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
          
          showNotification(paste0(input$new_sku, " 图片已成功更新！"), type = "message")
        }, error = function(e) {
          showNotification("图片路径更新失败！", type = "error")
        })
      } else {
        showNotification("未检测到有效的图片数据！", type = "error")
      }
    } else {
      # SKU 不存在
      showNotification("库存中无此 SKU 商品，无法更新图片！", type = "error")
    }
    
    # 重置
    shinyjs::reset("new_item_image")
    uploaded_file(NULL)
    pasted_file(NULL)
    output$pasted_image_preview <- renderUI({ NULL })
    shinyjs::show("paste_prompt")
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
        image_path <- added_items_df$ItemImagePath[i]  # 已经处理好的图片路径
        
        # 检查 SKU 是否已存在
        existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
        
        if (nrow(existing_item) > 0) {
          # SKU 已存在，更新库存
          new_quantity <- existing_item$Quantity + quantity
          new_ave_product_cost <- ((existing_item$ProductCost * existing_item$Quantity) + (product_cost * quantity)) / new_quantity
          new_ave_shipping_cost <- ((existing_item$ShippingCost * existing_item$Quantity) + (unit_shipping_cost * quantity)) / new_quantity
          
          dbExecute(con, "UPDATE inventory 
                        SET Quantity = ?, ProductCost = ?, ShippingCost = ? 
                        WHERE SKU = ?",
                    params = list(new_quantity, round(new_ave_product_cost, 2), round(new_ave_shipping_cost, 2), sku))
          
          showNotification(paste("库存数据更新成功! SKU:", sku, ", 商品名:", item_name), type = "message")
        } else {
          # SKU 不存在，插入新商品
          dbExecute(con, "INSERT INTO inventory 
                        (SKU, Maker, MajorType, MinorType, ItemName, Quantity, ProductCost, ShippingCost, ItemImagePath) 
                        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    params = list(sku, maker, major_type, minor_type, item_name, quantity, 
                                  round(product_cost, 2), round(unit_shipping_cost, 2), image_path))
          
          showNotification(paste("新商品成功加入库存数据! SKU:", sku, ", 商品名:", item_name), type = "message")
        }
      }
      
      # 更新 inventory 数据并触发 UI 刷新
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
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
        for (i in 1:nrow(batch_data)) {
          dbExecute(con, "INSERT INTO unique_items (UniqueID, SKU, ProductCost, DomesticShippingCost, Status, Defect, PurchaseTime) VALUES (?, ?, ?, ?, ?, ?, ?)",
                    unname(as.vector(batch_data[i, ])))
        }
        dbCommit(con)
        showNotification("所有采购货物已成功登记！", type = "message")
        unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger())
      }, error = function(e) {
        dbRollback(con)
        showNotification(paste("采购登记失败:", e$message), type = "error")
      })
      
      # Clear added items and reset input fields
      added_items(create_empty_inventory())
      shinyjs::reset("new_item_image")
      uploaded_file(NULL)
      pasted_file(NULL)
      output$pasted_image_preview <- renderUI({ NULL })
      shinyjs::show("paste_prompt")
    }, error = function(e) {
      showNotification(paste("发生错误:", e$message), type = "error")
    })
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
      # 仅处理最后一个选择的行
      last_selected <- tail(selected_row, 1) # 获取最后一个选择的行号
      selected_data <- added_items()[last_selected, ] # 提取最后一个选择的数据
      
      # 更新侧边栏的输入字段
      updateSelectInput(session, "new_maker", selected = selected_data$Maker)
      updateSelectInput(session, "new_major_type", selected = selected_data$MajorType)
      updateSelectInput(session, "new_minor_type", selected = selected_data$MinorType)
      updateTextInput(session, "new_name", value = selected_data$ItemName)
      updateNumericInput(session, "new_quantity", value = selected_data$Quantity)
      updateNumericInput(session, "new_product_cost", value = selected_data$ProductCost)
    }
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
      output$pasted_image_preview <- renderUI({ NULL })
      
      # 通知用户
      showNotification("输入已清空！", type = "message")
    }, error = function(e) {
      # 捕获错误并通知用户
      showNotification("清空输入时发生错误，请重试！", type = "error")
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
  ## 物品编辑模块                                               ##
  ##                                                            ##
  ################################################################
  
  observeEvent(input$confirm_delete_btn, {
    selected_rows <- unique_items_table_manage_selected_row()
    
    # 如果没有选中行，提示用户
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("请先选择要删除的物品！", type = "error")
      return()
    }
    
    tryCatch({
      # 获取选中物品的 UniqueID 和 SKU
      selected_items <- unique_items_data()[selected_rows, ]
      
      dbBegin(con) # 开启事务
      
      for (i in seq_len(nrow(selected_items))) {
        # 删除 unique_items 中对应的记录
        dbExecute(con, "
        DELETE FROM unique_items
        WHERE UniqueID = ?", params = list(selected_items$UniqueID[i]))
        
        # 重新计算平均 ProductCost 和 ShippingCost
        sku <- selected_items$SKU[i]
        
        remaining_items <- dbGetQuery(con, "
        SELECT AVG(ProductCost) AS AvgProductCost, 
               AVG(DomesticShippingCost) AS AvgShippingCost,
               COUNT(*) AS RemainingCount
        FROM unique_items
        WHERE SKU = ?", params = list(sku))
        
        if (remaining_items$RemainingCount[1] > 0) {
          # 更新 inventory 表的平均成本和库存数量
          dbExecute(con, "
          UPDATE inventory
          SET Quantity = ?, 
              ProductCost = ?, 
              ShippingCost = ?
          WHERE SKU = ?", 
                    params = list(
                      remaining_items$RemainingCount[1],
                      remaining_items$AvgProductCost[1],
                      remaining_items$AvgShippingCost[1],
                      sku
                    ))
        } else {
          # 如果没有剩余记录，删除 inventory 表中的该 SKU
          dbExecute(con, "
          DELETE FROM inventory
          WHERE SKU = ?", params = list(sku))
        }
      }
      
      dbCommit(con) # 提交事务
      
      showNotification("物品删除成功！", type = "message")
      
      # 更新inventory()，触发filtered_inventory_table更新
      inventory(dbGetQuery(con, "SELECT * FROM inventory"))
      
      # 触发更新刷新unique_items_data
      unique_items_data_refresh_trigger(!unique_items_data_refresh_trigger()) 
      
    }, error = function(e) {
      dbRollback(con) # 回滚事务
      showNotification(paste("删除物品时发生错误：", e$message), type = "error")
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
  
  # 监听选中行并更新 SKU
  observeEvent(unique_items_table_defect_selected_row(), {
    if (!is.null(unique_items_table_defect_selected_row()) && length(unique_items_table_defect_selected_row()) > 0) {
      selected_sku <- unique_items_data()[unique_items_table_defect_selected_row(), "SKU", drop = TRUE]
      updateTextInput(session, "defect_sku", value = selected_sku)
      updateTextInput(session, "repair_sku", value = selected_sku)
    }
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
  
  observe({
    sku <- trimws(input$query_sku)
    
    if (sku == "") {
      output$query_item_info <- renderUI({ div() })
      output$inventory_status_chart <- renderPlotly({ NULL })
      output$defect_status_chart <- renderPlotly({ NULL })
      return()
    }
    
    tryCatch({
      # 查询 SKU 数据
      sku_query <- "
      SELECT
        ItemName, Maker, MajorType, MinorType, Quantity,
        ProductCost, ShippingCost, ItemImagePath
      FROM inventory
      WHERE SKU = ?"
      sku_data <- dbGetQuery(con, sku_query, params = list(sku))
      
      if (nrow(sku_data) == 0) {
        output$query_item_info <- renderUI({
          div(tags$p("未找到该 SKU 对应的商品信息！", style = "color: red; font-size: 16px;"))
        })
        return()
      }
      
      output$query_item_info <- renderUI({
        img_path <- ifelse(
          is.na(sku_data$ItemImagePath[1]),
          placeholder_300px_path,
          paste0(host_url, "/images/", basename(sku_data$ItemImagePath[1]))
        )
        div(
          style = "display: flex; align-items: center; padding: 10px;",
          div(style = "flex: 1; text-align: center; margin-right: 20px;",
              tags$img(src = img_path, height = "300px", style = "border: 1px solid #ddd; border-radius: 8px;")),
          div(style = "flex: 2; display: flex; flex-direction: column; justify-content: center;",
              tags$p(tags$b("商品名称："), sku_data$ItemName[1]),
              tags$p(tags$b("供应商："), sku_data$Maker[1]),
              tags$p(tags$b("分类："), paste(sku_data$MajorType[1], "/", sku_data$MinorType[1])),
              tags$p(tags$b("库存数量："), sku_data$Quantity[1]),
              tags$p(tags$b("平均成本："), sprintf("¥%.2f", sku_data$ProductCost[1])),
              tags$p(tags$b("平均运费："), sprintf("¥%.2f", sku_data$ShippingCost[1]))
          )
        )
      })
      
      # 渲染库存状态图表
      output$inventory_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          inventory_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Status) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 定义固定类别顺序和颜色
          status_levels <- c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货")
          status_colors <- c("lightgray", "#c7e89b", "#4B4B4B", "#46a80d", "#173b02", "#A9A9A9", "red")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          inventory_status_data <- merge(
            data.frame(Status = status_levels),
            inventory_status_data,
            by = "Status",
            all.x = TRUE
          )
          inventory_status_data$Count[is.na(inventory_status_data$Count)] <- 0
          
          # 按 status_levels 排序，确保颜色对应
          inventory_status_data <- inventory_status_data[match(status_levels, inventory_status_data$Status), ]
          
          if (sum(inventory_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = inventory_status_data,
              labels = ~Status,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = status_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = TRUE, # 显示图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("库存状态图表生成错误：", e$message), type = "error")
          output$inventory_status_chart <- renderPlotly({ NULL })
        })
      })
      
      # 渲染瑕疵情况图表
      output$defect_status_chart <- renderPlotly({
        tryCatch({
          data <- unique_items_data()
          
          if (is.null(input$query_sku) || trimws(input$query_sku) == "") {
            return(NULL)
          }
          
          # 筛选符合条件的数据
          defect_status_data <- data %>%
            filter(SKU == trimws(input$query_sku)) %>%
            group_by(Defect) %>%
            summarise(Count = n(), .groups = "drop")
          
          # 定义固定类别顺序和颜色
          defect_levels <- c("未知", "无瑕", "瑕疵", "修复")
          defect_colors <- c("darkgray", "green", "red", "orange")
          
          # 确保数据按照固定类别顺序排列，并用 0 填充缺失类别
          defect_status_data <- merge(
            data.frame(Defect = defect_levels),
            defect_status_data,
            by = "Defect",
            all.x = TRUE
          )
          defect_status_data$Count[is.na(defect_status_data$Count)] <- 0
          
          # 按 defect_levels 排序，确保颜色对应
          defect_status_data <- defect_status_data[match(defect_levels, defect_status_data$Defect), ]
          
          if (sum(defect_status_data$Count) == 0) {
            # 如果没有数据，显示占位图
            plot_ly(type = "pie", labels = c("无数据"), values = c(1), textinfo = "label+value")
          } else {
            # 渲染饼图
            plot_ly(
              data = defect_status_data,
              labels = ~Defect,
              values = ~Count,
              type = "pie",
              textinfo = "label+value",       # 图上显示类别和数量
              hoverinfo = "label+percent+value", # 鼠标悬停显示类别、百分比和数量
              insidetextorientation = "auto", # 自动调整标签方向
              textposition = "inside",       # 标签显示在图形外部
              marker = list(colors = defect_colors) # 按固定颜色映射
            ) %>%
              layout(
                showlegend = TRUE, # 显示图例
                margin = list(l = 20, r = 20, t = 30, b = 30), # 增加边距
                uniformtext = list(minsize = 10, mode = "hide") # 统一文本大小
              )
          }
        }, error = function(e) {
          showNotification(paste("瑕疵情况图表生成错误：", e$message), type = "error")
          output$defect_status_chart <- renderPlotly({ NULL })
        })
      })
      
    }, error = function(e) {
      showNotification(paste("发生错误：", e$message), type = "error")
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