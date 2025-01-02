itemFilterServer <- function(id, makers_df, unique_items_data, filtered_unique_items_data, unique_items_table_selected_row) {
  moduleServer(id, function(input, output, session) {
    # 缓存 makers 和 item_names 的哈希值
    makers_hash <- reactiveVal(NULL)
    item_names_hash <- reactiveVal(NULL)
    
    # 更新 makers 控件
    observe({
      current_makers <- makers_df()
      new_hash <- digest::digest(current_makers)
      
      # 如果 makers 数据未变化，则不更新
      if (!is.null(makers_hash()) && makers_hash() == new_hash) return()
      
      makers_hash(new_hash)
      updateSelectizeInput(
        session, "maker", 
        choices = c("", setNames(current_makers$Maker, paste0(current_makers$Maker, "(", current_makers$Pinyin, ")"))), 
        selected = "", server = TRUE
      )
    })
    
    # 动态更新商品名称
    observe({
      selected_makers <- input$maker
      filtered_data <- if (!is.null(selected_makers) && selected_makers != "") {
        unique_items_data() %>% filter(Maker %in% as.character(selected_makers))
      } else {
        unique_items_data()
      }
      
      new_hash <- digest::digest(filtered_data$ItemName)
      
      # 如果 item_names 未变化，则不更新
      if (!is.null(item_names_hash()) && item_names_hash() == new_hash) return()
      
      item_names_hash(new_hash)
      item_names <- c("", unique(filtered_data$ItemName))
      updateSelectizeInput(session, "name", choices = item_names, selected = "")
    })
    
    # 清空输入
    observeEvent(input$reset_btn, {
      tryCatch({
        updateSelectizeInput(
          session, "maker", 
          choices = c("", setNames(makers_df()$Maker, paste0(makers_df()$Maker, "(", makers_df()$Pinyin, ")"))), 
          selected = "", server = TRUE
        )        
        updateSelectizeInput(session, "name", choices = c(""), selected = "")
        updateDateRangeInput(session, "purchase_date_range", start = Sys.Date() - 365, end = Sys.Date())
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    })
  })
}
