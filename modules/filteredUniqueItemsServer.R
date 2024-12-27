filteredUniqueItemsServer <- function(id, unique_items_data, status_filter = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 创建过滤后的数据
    filtered_data <- reactive({
      req(unique_items_data())
      data <- unique_items_data()
      
      # 应用状态过滤条件（如果有）
      if (!is.null(status_filter)) {
        data <- data[data$Status %in% status_filter, ]
      }
      
      # 使用 itemFilterServer 的输入进行动态筛选
      filter_unique_items_data_by_inputs(
        data = data,
        input = input,
        maker_input_id = ns("maker"),
        item_name_input_id = ns("name")
      )
    })
    
    # 返回过滤后的数据
    return(filtered_data)
  })
}
