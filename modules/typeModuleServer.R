typeModuleServer <- function(id, con, item_type_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 为模块创建命名空间
    
    # 渲染大类下拉框
    output$major_type_ui <- renderUI({
      type_data <- item_type_data()
      
      if (is.null(type_data) || nrow(type_data) == 0) {
        selectInput(ns("new_major_type"), "大类:", choices = c("暂无数据" = ""), selected = NULL)
      } else {
        choices <- setNames(
          unique(type_data$MajorType), 
          paste0(unique(type_data$MajorType), "（", unique(type_data$MajorTypeSKU), "）")
        )
        selectInput(ns("new_major_type"), "大类:", choices = choices, selected = NULL)
      }
    })
    
    # 渲染小类下拉框
    output$minor_type_ui <- renderUI({
      type_data <- item_type_data()
      selected_major <- if (!is.null(input$new_major_type)) gsub("（.*）", "", input$new_major_type) else NULL
      
      if (is.null(type_data) || nrow(type_data) == 0 || is.null(selected_major)) {
        selectInput(ns("new_minor_type"), "小类:", choices = c("暂无数据" = ""), selected = NULL)
      } else {
        filtered_data <- type_data[type_data$MajorType == selected_major, ]
        choices <- setNames(
          filtered_data$MinorType, 
          paste0(filtered_data$MinorType, "（", filtered_data$MinorTypeSKU, "）")
        )
        selectInput(ns("new_minor_type"), "小类:", choices = choices, selected = NULL)
      }
    })
    
    # 新增大类逻辑
    observeEvent(input$add_major_type_btn, {
      showModal(modalDialog(
        title = "新增大类",
        textInput(ns("new_major_type_name"), "大类名称:"),
        textInput(ns("new_major_type_sku"), "大类SKU:"),
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("confirm_add_major_type"), "添加")
        )
      ))
    })
    
    observeEvent(input$confirm_add_major_type, {
      req(input$new_major_type_name, input$new_major_type_sku)
      
      new_major <- data.frame(
        MajorType = input$new_major_type_name,
        MajorTypeSKU = input$new_major_type_sku,
        stringsAsFactors = FALSE
      )
      
      tryCatch({
        dbExecute(con, "INSERT INTO item_type_data (MajorType, MajorTypeSKU) VALUES (?, ?)",
                  params = list(new_major$MajorType, new_major$MajorTypeSKU))
        showNotification("新增大类成功！", type = "message")
        removeModal()
        item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
        updateSelectInput(session, "new_major_type", selected = new_major$MajorType)
      }, error = function(e) {
        showNotification("新增大类失败。", type = "error")
      })
    })
    
    # 新增小类逻辑
    observeEvent(input$add_minor_type_btn, {
      req(input$new_major_type)
      
      selected_major <- gsub("（.*）", "", input$new_major_type)
      
      showModal(modalDialog(
        title = paste0("批量新增小类（大类: ", selected_major, "）"),
        textAreaInput(ns("new_minor_types"), "小类名称（每行一个）:", placeholder = "输入小类名称，每行一个"),
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("confirm_add_minor_types"), "批量添加")
        )
      ))
    })
    
    observeEvent(input$confirm_add_minor_types, {
      req(input$new_minor_types, input$new_major_type)
      
      # 获取选择的大类名称（去除 SKU 部分）
      selected_major <- gsub("（.*）", "", input$new_major_type)
      
      # 查询大类 SKU
      major_sku <- tryCatch({
        type_data <- item_type_data()
        type_row <- type_data[type_data$MajorType == selected_major, ]
        if (nrow(type_row) > 0) type_row$MajorTypeSKU[1] else NA
      }, error = function(e) {
        NA
      })
      
      req(!is.na(major_sku))  # 确保大类 SKU 存在
      
      # 解析用户输入的小类名称
      minor_type_names <- strsplit(input$new_minor_types, "\n")[[1]]
      minor_type_names <- trimws(minor_type_names)  # 去除多余空格
      minor_type_names <- minor_type_names[minor_type_names != ""]  # 去除空行
      
      if (length(minor_type_names) == 0) {
        showNotification("请输入至少一个有效的小类名称！", type = "error")
        return()
      }
      
      tryCatch({
        # 批量处理小类
        for (minor_name in minor_type_names) {
          # 校验名称有效性
          if (is.null(minor_name) || minor_name == "") {
            next  # 跳过无效行
          }
          
          # 自动生成 SKU
          minor_sku <- generate_unique_code(minor_name, length = 2)
          
          # 插入到数据库
          dbExecute(con, 
                    "INSERT INTO item_type_data (MajorType, MajorTypeSKU, MinorType, MinorTypeSKU) VALUES (?, ?, ?, ?)",
                    params = list(selected_major, major_sku, minor_name, minor_sku))
        }
        
        # 删除与大类关联的小类为空的行
        dbExecute(con, "DELETE FROM item_type_data WHERE MajorType = ? AND (MinorType IS NULL OR MinorType = '')",
                  params = list(selected_major))
        
        showNotification("批量新增小类成功！", type = "message")
        removeModal()
        
        # 重新加载数据
        item_type_data(dbGetQuery(con, "SELECT * FROM item_type_data"))
        updateSelectInput(session, "new_major_type", selected = selected_major)
      }, error = function(e) {
        showNotification(paste("批量新增小类失败：", e$message), type = "error")
        print(e)  # 打印错误详情到控制台
      })
    })
  })
}
