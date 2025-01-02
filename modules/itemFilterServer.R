itemFilterServer <- function(id, unique_items_data) {
  moduleServer(id, function(input, output, session) {
    # 缓存 makers 和 item_names 的哈希值
    makers_hash <- reactiveVal(NULL)
    item_names_hash <- reactiveVal(NULL)
    
    # 动态更新 makers 控件
    observe({
      req(unique_items_data())  # 确保数据已加载
      
      # 提取 unique_items_data() 中 Maker 列的唯一值
      current_makers <- unique_items_data() %>% pull(Maker) %>% unique()
      new_hash <- digest::digest(current_makers)
      
      # 如果 Maker 的哈希值没有变化，则不更新
      if (!is.null(makers_hash()) && makers_hash() == new_hash) return()

      makers_hash(new_hash)
      makers_df <- if (!is.null(current_makers) && length(current_makers) > 0) {
        data.frame(Maker = current_makers, stringsAsFactors = FALSE) %>%
          mutate(Pinyin = remove_tone(stringi::stri_trans_general(Maker, "Latin")))
      } else {
        data.frame(Maker = character(), Pinyin = character(), stringsAsFactors = FALSE)
      }
      
      updateSelectizeInput(
        session, "maker", 
        choices = c("", setNames(makers_df$Maker, paste0(makers_df$Maker, "(", makers_df$Pinyin, ")"))), 
        selected = "", server = TRUE
      )
    })
    

    # 动态更新商品名称
    observe({
      req(unique_items_data())  # 确保数据已加载
      
      selected_makers <- input$maker
      filtered_data <- if (!is.null(selected_makers) && selected_makers != "") {
        unique_items_data() %>% filter(Maker %in% as.character(selected_makers))
      } else {
        unique_items_data()
      }
      
      # 提取 ItemName 的唯一值并排序
      current_item_names <- filtered_data$ItemName %>% unique() %>% sort()
      new_hash <- digest::digest(current_item_names)
      
      # 如果 item_names 未变化，则不更新
      if (!is.null(item_names_hash()) && item_names_hash() == new_hash) return() 
      
      item_names_hash(new_hash)
      updateSelectizeInput(session, "name", choices = c("", current_item_names), selected = "")
    })
    
    # 清空输入
    observeEvent(input$reset_btn, {
      tryCatch({
        # 重置 makers 控件
        # current_makers <- unique_items_data() %>% pull(Maker) %>% unique()
        # makers_df <- if (!is.null(current_makers) && length(current_makers) > 0) {
        #   data.frame(Maker = current_makers, stringsAsFactors = FALSE) %>%
        #     mutate(Pinyin = remove_tone(stringi::stri_trans_general(Maker, "Latin")))
        # } else {
        #   data.frame(Maker = character(), Pinyin = character(), stringsAsFactors = FALSE)
        # }
        # updateSelectizeInput(
        #   session, "maker",
        #   choices = c("", setNames(makers_df$Maker, paste0(makers_df$Maker, "(", makers_df$Pinyin, ")"))),
        #   selected = "", server = TRUE
        # )
        # 
        updateSelectizeInput(session, "maker", selected = NULL)
        
        
        # 重置商品名称控件
        updateSelectizeInput(session, "name", choices = c(""), selected = "")
        updateDateRangeInput(session, "purchase_date_range", start = Sys.Date() - 365, end = Sys.Date())
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    })
  })
}
