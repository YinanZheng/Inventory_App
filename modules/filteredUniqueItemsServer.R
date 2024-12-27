filteredUniqueItemsServer <- function(id, unique_items, status_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 返回过滤后的数据
    reactive({
      req(unique_items())  # 确保数据存在
      
      # 监听用户选择的供应商和商品名
      selected_maker <- input$maker
      selected_name <- input$name
      
      # 显示用户选择的调试信息
      showNotification(paste("Selected Maker:", selected_maker))
      showNotification(paste("Selected Name:", selected_name))
      
      # 执行过滤逻辑
      filtered_data <- unique_items() %>%
        filter(Status %in% as.character(status_filter)) %>%
        filter_unique_items_data_by_inputs(
          data = .,
          input = input,
          maker_input_id = ns("maker"),
          item_name_input_id = ns("name")
        )
      
      # 显示过滤后的结果
      if (nrow(filtered_data) > 0) {
        showNotification(paste("Filtered Items:", paste(filtered_data$ItemName, collapse = ", ")))
      } else {
        showNotification("No data matches the filter.")
      }
      
      # 返回过滤后的数据
      filtered_data
    })
  })
}
