itemFilterServer <- function(id, unique_items, makers, selected_row_reactive = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 动态生成命名空间
    
    # 更新供应商名称
    observeEvent(makers(), {
      tryCatch({
        showNotification(ns("maker"))
        showNotification(makers()$Maker)
        
        update_maker_choices(session, ns("maker"), makers())
      }, error = function(e) {
        # 捕获并显示错误信息
        message("Error in updating maker choices: ", e$message)
        showNotification(paste("Error: ", e$message), type = "error")
      })
    })
    
    # 动态更新商品名称
    observe({
      tryCatch({
        req(unique_items())  # 确保数据存在
        
        # 获取用户选择的供应商
        selected_maker <- input$maker
        
        # 根据供应商筛选商品名称
        filtered_data <- if (!is.null(selected_maker) && selected_maker != "") {
          unique_items() %>% filter(Maker %in% selected_maker)
        } else {
          unique_items()
        }
        
        item_names <- c("", filtered_data %>% pull(ItemName) %>% unique())  # 添加空选项
        
        # 更新商品名称输入框
        updateSelectizeInput(session, ns("name"), choices = item_names, selected = "")
      }, error = function(e) {
        # 捕获并显示错误信息
        message("Error in updating item names: ", e$message)
        showNotification(paste("Error: ", e$message), type = "error")
      })
    })
    
    # 监听选中行并更新 Maker、商品名称和 SKU
    observeEvent(selected_row_reactive(), {
      tryCatch({
        selected_row <- selected_row_reactive()
        if (!is.null(selected_row) && length(selected_row) > 0) {
          selected_data <- unique_items()[selected_row, ]
          updateSelectizeInput(session, ns("maker"), selected = selected_data$Maker)
          shinyjs::delay(300, {
            updateTextInput(session, ns("name"), value = selected_data$ItemName)
          })
          updateTextInput(session, ns("sku"), value = selected_data$SKU)
        }
      }, error = function(e) {
        # 捕获并显示错误信息
        message("Error in updating selected row: ", e$message)
        showNotification(paste("Error: ", e$message), type = "error")
      })
    })
    
    # 清空筛选条件
    observeEvent(input$reset_btn, {
      tryCatch({
        updateSelectizeInput(session, ns("maker"), choices = makers(), selected = NULL)
        updateTextInput(session, ns("name"), value = "")
        updateTextInput(session, ns("sku"), value = "")
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        # 捕获并显示错误信息
        message("Error in resetting filters: ", e$message)
        showNotification(paste("Error: ", e$message), type = "error")
      })
    })
  })
}
