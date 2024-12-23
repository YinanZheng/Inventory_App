# 物品筛选模块 Server
itemFilterServer <- function(id, makers_df, unique_items_data, filtered_unique_items_data, unique_items_table_selected_row, label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 更新供应商名称
    observeEvent(makers_df(), {
      update_maker_choices(session, ns(paste0(label, "_maker")), makers_df())
    })
    
    # 监听供应商选择变化并动态更新商品名称
    observe({
      req(unique_items_data())  # 确保数据存在
      
      # 获取用户选择的供应商
      selected_makers <- input[[paste0(label, "_maker")]]
      
      # 筛选商品名称
      if (!is.null(selected_makers) && length(selected_makers) > 0) {
        filtered_data <- unique_items_data() %>% filter(Maker %in% selected_makers)
      } else {
        filtered_data <- unique_items_data()
      }
      
      # 提取对应的商品名称，并在前面加一个空选项
      item_names <- c("", filtered_data %>% pull(ItemName) %>% unique())
      
      # 更新商品名称选项，默认选中空选项
      updateSelectizeInput(session, ns(paste0(label, "_name")), choices = item_names, selected = "")
    })
    
    # 监听选中行并更新 maker, item name, SKU
    observeEvent(unique_items_table_selected_row(), {
      if (!is.null(unique_items_table_selected_row()) && length(unique_items_table_selected_row()) > 0) {
        selected_data <- filtered_unique_items_data()[unique_items_table_selected_row(), ]
        updateSelectInput(session, ns(paste0(label, "_maker")), selected = selected_data$Maker)
        shinyjs::delay(300, {  # 延迟 300 毫秒
          updateTextInput(session, ns(paste0(label, "_name")), value = selected_data$ItemName)
        })
        updateTextInput(session, ns(paste0(label, "_sku")), value = selected_data$SKU)
      }
    })
    
    # 清空输入
    observeEvent(input[[paste0(label, "_reset_btn")]], {
      tryCatch({
        update_maker_choices(session, ns(paste0(label, "_maker")), makers_df())
        updateTextInput(session, ns(paste0(label, "_name")), value = "")
        updateTextInput(session, ns(paste0(label, "_sku")), value = "")
        
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    })
  })
}
