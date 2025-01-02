itemFilterServer <- function(id, makers_df, unique_items_data, filtered_unique_items_data, unique_items_table_selected_row) {
  moduleServer(id, function(input, output, session) {
    # 缓存上次的 makers 和 unique_items_data 的哈希值
    makers_hash <- reactiveVal(NULL)
    unique_items_hash <- reactiveVal(NULL)
    
    # 更新 makers 控件
    observe({
      current_makers <- makers_df()
      new_hash <- digest::digest(current_makers)
      
      # 如果 makers 数据未变化，则不更新
      if (!is.null(makers_hash()) && makers_hash() == new_hash) return()
      
      # 更新缓存并更新 UI
      makers_hash(new_hash)
      updateSelectizeInput(
        session, "maker", 
        choices = c("", setNames(current_makers$Maker, paste0(current_makers$Maker, "(", current_makers$Pinyin, ")"))), 
        selected = "", server = TRUE
      )
    })
    
    # 动态更新商品名称
    observe({
      current_unique_items <- unique_items_data()
      new_hash <- digest::digest(current_unique_items)
      
      # 如果 unique_items_data 数据未变化，则不更新
      if (!is.null(unique_items_hash()) && unique_items_hash() == new_hash) return()
      
      # 更新缓存并过滤数据
      unique_items_hash(new_hash)
      selected_makers <- input$maker
      filtered_data <- if (!is.null(selected_makers) && selected_makers != "") {
        current_unique_items %>% filter(Maker %in% as.character(selected_makers))
      } else {
        current_unique_items
      }
      
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
