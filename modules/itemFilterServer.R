itemFilterServer <- function(id, unique_items_data, makers_df, selected_row_reactive = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 更新供应商名称
    observeEvent(makers_df(), {
      updateSelectizeInput(session, ns("maker"), choices = makers_df(), selected = NULL)
    })
    
    # 动态更新商品名称
    observe({
      req(unique_items_data())  # 确保数据存在
      
      # 获取用户选择的供应商
      selected_makers <- input$maker
      
      # 根据供应商筛选商品名称
      filtered_data <- if (!is.null(selected_makers) && length(selected_makers) > 0) {
        unique_items_data() %>% filter(Maker %in% selected_makers)
      } else {
        unique_items_data()
      }
      
      item_names <- c("", filtered_data %>% pull(ItemName) %>% unique())  # 添加空选项
      
      # 更新商品名称输入框
      updateSelectizeInput(session, ns("name"), choices = item_names, selected = "")
    })
    
    # 监听选中行并更新 Maker、商品名称和 SKU
    observeEvent(selected_row_reactive(), {
      selected_row <- selected_row_reactive()
      if (!is.null(selected_row) && length(selected_row) > 0) {
        selected_data <- unique_items_data()[selected_row, ]
        updateSelectizeInput(session, ns("maker"), selected = selected_data$Maker)
        shinyjs::delay(300, {
          updateTextInput(session, ns("name"), value = selected_data$ItemName)
        })
        updateTextInput(session, ns("sku"), value = selected_data$SKU)
      }
    })
    
    # 清空筛选条件
    observeEvent(input$reset_btn, {
      tryCatch({
        updateSelectizeInput(session, ns("maker"), choices = makers_df(), selected = NULL)
        updateTextInput(session, ns("name"), value = "")
        updateTextInput(session, ns("sku"), value = "")
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    })
  })
}
