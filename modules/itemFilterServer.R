itemFilterServer <- function(id, unique_items_data) {
  moduleServer(id, function(input, output, session) {
    # 缓存 makers 和 item_names 的哈希值
    makers_hash <- reactiveVal(NULL)
    item_names_hash <- reactiveVal(NULL)
    
    # 动态更新 makers 控件
    observe({
      req(unique_items_data())  # 确保数据已加载
      
      current_makers <- unique_items_data() %>% pull(Maker) %>% unique()
      new_hash <- digest::digest(current_makers)
      
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
      
      current_item_names <- filtered_data$ItemName %>% unique() %>% sort()
      new_hash <- digest::digest(current_item_names)
      
      if (!is.null(item_names_hash()) && item_names_hash() == new_hash) return()
      
      item_names_hash(new_hash)
      updateSelectizeInput(session, "name", choices = c("", current_item_names), selected = "")
    })
    
    # 清空输入（按钮绑定逻辑）
    observeEvent(input$reset_btn, {
      resetFilters()  # 调用封装的 resetFilters 方法
    })
    
    # 清空输入的逻辑封装为 resetFilters 方法
    resetFilters <- function() {
      tryCatch({
        # 重置 makers 控件
        # current_makers <- unique_items_data() %>% pull(Maker) %>% unique()
        # makers_df <- if (!is.null(current_makers) && length(current_makers) > 0) {
        #   data.frame(Maker = current_makers, stringsAsFactors = FALSE) %>%
        #     mutate(Pinyin = remove_tone(stringi::stri_trans_general(Maker, "Latin")))
        # } else {
        #   data.frame(Maker = character(), Pinyin = character(), stringsAsFactors = FALSE)
        # }
        # 
        # updateSelectizeInput(
        #   session, "maker",
        #   choices = c("", setNames(makers_df$Maker, paste0(makers_df$Maker, "(", makers_df$Pinyin, ")"))),
        #   selected = NULL, server = TRUE
        # )
        # 
        updateSelectizeInput(
          session, 
          inputId = "maker",
          selected = NULL, 
          server = TRUE
        )
        
        # 重置商品名称控件
        current_item_names <- unique_items_data()$ItemName %>% unique() %>% sort()
        updateSelectizeInput(session, "name", choices = c("", current_item_names), selected = NULL)
        
        # 重置日期选择器
        updateDateRangeInput(session, "purchase_date_range", start = Sys.Date() - 365, end = Sys.Date())
        
        showNotification("筛选条件已重置！", type = "message")
      }, error = function(e) {
        showNotification("重置输入时发生错误，请重试！", type = "error")
      })
    }
    
    # 返回 resetFilters 方法供外部调用
    return(list(resetFilters = resetFilters))
  })
}
