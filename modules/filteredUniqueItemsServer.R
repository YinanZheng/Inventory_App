filteredUniqueItemsServer <- function(id, unique_items, status_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    reactive({
      req(unique_items())  # 确保数据存在
      
      # 显示过滤器和用户输入的调试信息
      showNotification("Filtering based on user inputs and status...")
      showNotification(paste("Status Filter:", paste(status_filter, collapse = ", ")))
      
      # 执行过滤逻辑
      filtered_data <- unique_items() %>%
        filter(Status %in% as.character(status_filter)) %>%
        filter_unique_items_data_by_inputs(
          data = .,
          input = input,
          maker_input_id = ns("maker"),
          item_name_input_id = ns("name")
        )
      
      # 显示过滤结果
      if (nrow(filtered_data) > 0) {
        showNotification("Filtered Data Updated:")
        showNotification(paste(filtered_data$ItemName, collapse = ", "))
      } else {
        showNotification("No data found matching the filter.")
      }
      
      filtered_data
    })
  })
}
