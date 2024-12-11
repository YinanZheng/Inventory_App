# Define server logic
server <- function(input, output, session) {
  ## Database
  con <- db_connection()
  
  ## 大小类模块
  
  # ReactiveVal 存储 item_type_data 数据
  item_type_data <- reactiveVal()
  
  # ReactiveVal 用于存储 inventory 数据
  inventory <- reactiveVal()
  
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
  
  
  #################################################################
  
  
  # Render Major Type Dropdown
  output$major_type_ui <- renderUI({
    req(item_type_data())  # Ensure item_type_data is not NULL
    
    # Extract unique major types with their SKUs
    type_data <- item_type_data()
    unique_majors <- unique(type_data[, c("MajorType", "MajorTypeSKU")])
    
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
    
    # Generate choices for the dropdown
    choices <- setNames(
      filtered_data$MinorType, 
      paste0(filtered_data$MinorType, "（", filtered_data$MinorTypeSKU, "）")
    )
    
    selectInput("new_minor_type", "小类:", choices = choices, selected = NULL)
  })
  
  # Add Major Type Button Logic
  observeEvent(input$add_major_type_btn, {
    showModal(modalDialog(
      title = "新增大类",
      textInput("new_major_type_name", "大类名称:"),
      textInput("new_major_type_sku", "大类SKU:"),
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_add_major_type", "添加")
      )
    ))
  })
  
  observeEvent(input$confirm_add_major_type, {
    req(input$new_major_type_name, input$new_major_type_sku)
    
    # 新增大类数据
    new_major <- data.frame(
      MajorType = input$new_major_type_name,
      MajorTypeSKU = input$new_major_type_sku,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      # 将新大类写入数据库，仅包含 MajorType 和 MajorTypeSKU 列
      dbExecute(con, 
                "INSERT INTO item_type_data (MajorType, MajorTypeSKU) VALUES (?, ?)",
                params = list(new_major$MajorType, new_major$MajorTypeSKU))
      
      showNotification("新增大类成功！", type = "message")
      removeModal()
      
      # 重新加载数据
      item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
      # 动态更新 new_major_type 的选择
      updateSelectInput(session, "new_major_type", selected = new_major$MajorType)
      
    }, error = function(e) {
      showNotification("新增大类失败。", type = "error")
    })
  })
  
  
  # Add Minor Type Button Logic
  observeEvent(input$add_minor_type_btn, {
    req(input$new_major_type)
    
    selected_major <- gsub("（.*）", "", input$new_major_type)
    
    showModal(modalDialog(
      title = paste0("新增小类（大类: ", selected_major, "）"),
      textInput("new_minor_type_name", "小类名称:"),
      textInput("new_minor_type_sku", "小类SKU:"),
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_add_minor_type", "添加")
      )
    ))
  })
  
  observeEvent(input$confirm_add_minor_type, {
    req(input$new_minor_type_name, input$new_minor_type_sku, input$new_major_type)
    
    # 从输入中获取选择的大类名称（去除SKU部分）
    selected_major <- gsub("（.*）", "", input$new_major_type)
    
    # 查询大类SKU
    major_sku <- tryCatch({
      data <- item_type_data()
      type_row <- data[data$MajorType == selected_major, ]
      if (nrow(type_row) > 0) type_row$MajorTypeSKU[1] else NA
    }, error = function(e) {
      NA
    })
    
    req(!is.na(major_sku)) # 确保大类SKU存在
    
    # 新增小类数据
    new_minor <- data.frame(
      MajorType = selected_major,
      MajorTypeSKU = major_sku,
      MinorType = input$new_minor_type_name,
      MinorTypeSKU = input$new_minor_type_sku,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      # 插入新小类到数据库
      dbExecute(con, 
                "INSERT INTO item_type_data (MajorType, MajorTypeSKU, MinorType, MinorTypeSKU) VALUES (?, ?, ?, ?)",
                params = list(new_minor$MajorType, new_minor$MajorTypeSKU, new_minor$MinorType, new_minor$MinorTypeSKU))
      
      # 删除与大类关联的小类为空的行
      dbExecute(con, 
                "DELETE FROM item_type_data WHERE MajorType = ? AND (MinorType IS NULL OR MinorType = '')",
                params = list(selected_major))
      
      showNotification("新增小类成功！", type = "message")
      removeModal()
      
      # 重新加载数据
      item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
      # 动态更新 new_major_type 的选择
      updateSelectInput(session, "new_major_type", selected = selected_major)
      
    }, error = function(e) {
      showNotification("新增小类失败。", type = "error")
    })
  })
  
  
  
  #########################################################################
  
  
  # 用于保存用户上传的文件信息
  uploaded_file <- reactiveVal(NULL)
  
  observeEvent(input$new_item_image, {
    if (!is.null(input$new_item_image)) {
      # 记录新上传的文件
      uploaded_file(input$new_item_image)
      showNotification("文件已上传并记录！", type = "message")
    }
  })
  
  # 声明一个 reactiveVal 用于触发表格刷新
  refresh_trigger <- reactiveVal(FALSE)
  
  
  observeEvent(input$toggle_inventory_table, {
    shinyjs::toggle("inventory_table_container")  # 切换显示/隐藏
  })
  
  observeEvent(input$toggle_item_table, {
    shinyjs::toggle("item_table_container")  # 切换显示/隐藏
  })

  
  # Reactive value to store added items
  added_items <- reactiveVal(create_empty_inventory() %>% select(-ShippingCost))
  
  #########################################################################
  
  ## 供应商模块
  supplier_module(input, output, session, con)
  
  #########################################################################
  
  ## 库存表渲染模块
  
  # Filter inventory based on major and minor type
  filtered_inventory <- reactive({
    # 当 refresh_trigger 改变时触发更新
    refresh_trigger()
    
    req(input$new_major_type, input$new_minor_type)
    
    # Filter the inventory data
    result <- inventory() %>%
      filter(MajorType == input$new_major_type, MinorType == input$new_minor_type) %>%
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
        MajorType = input$new_major_type,
        MinorType = input$new_minor_type,
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
        MajorType = input$new_major_type,
        MinorType = input$new_minor_type,
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
  
  
  
  ########################################################################
  
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
    # 检查是否已生成条形码
    if (is.null(batch_pdf_file_path())) {
      showModal(modalDialog(
        title = "条形码未打印",
        "您还未生成并打印条形码，是否继续提交入库？",
        footer = tagList(
          modalButton("取消"),
          actionButton("force_confirm_btn", "继续提交", class = "btn-primary")
        )
      ))
    } else {
      # 已生成条形码，直接提交入库逻辑
      handle_inventory_submission()
    }
  })
  
  # 处理用户点击“继续提交”的逻辑
  observeEvent(input$force_confirm_btn, {
    removeModal()  # 关闭模态框
    handle_inventory_submission()  # 提交入库逻辑
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
    )
  })
  
  
  ### SKU 模块
  
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
  
  
  ### Barcode PDF 模块
  session$onFlushed(function() {
    shinyjs::disable("download_single_pdf")
    shinyjs::disable("download_batch_pdf")
  })
  
  # 用于存储 PDF 文件路径
  single_pdf_file_path <- reactiveVal(NULL)
  batch_pdf_file_path <- reactiveVal(NULL)
  
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
  
  # 用于存储撤回记录的队列
  undo_queue <- reactiveVal(list())  # 每次出库时记录 UniqueID 和状态
  
  # 监听条形码输入框
  observeEvent(input$outbound_sku, {
    sku <- input$outbound_sku
    
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