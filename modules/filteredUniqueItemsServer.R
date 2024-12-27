filteredUniqueItemsServer <- function(id, unique_items, status_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 返回过滤后的数据作为 reactive
    reactive({
      req(unique_items())  # 确保数据存在
      
      # 过滤逻辑
      filtered_data <- unique_items() %>%
        filter(Status %in% as.character(status_filter)) %>%
        filter_unique_items_data_by_inputs(
          data = .,
          input = input,
          maker_input_id = ns("maker"),
          item_name_input_id = ns("name")
        )
      
      # 返回过滤后的数据
      filtered_data
    })
  })
}
