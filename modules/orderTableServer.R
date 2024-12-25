orderTableServer <- function(input, output, session, column_mapping, selection = "single", data) {
  output$order_table <- renderDT({
    # 初始化渲染表
    datatable_and_names <- render_table_with_images(
      data = data(),                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection,
      image_column = "OrderImagePath" # 图片列映射
    )
    
    # 获取数据列名
    column_names <- datatable_and_names$column_names
    table <- datatable_and_names$datatable
    
    # 平台字段高亮
    if ("平台" %in% column_names) {
      table <- table %>%
        formatStyle(
          "平台",
          backgroundColor = styleEqual(
            c("Etsy", "Shopify", "TikTok", "其他"),
            c("#f45f0d", "#5a9a2a", "black", "lightgray")  # 不同平台的背景颜色
          ),
          color = styleEqual(
            c("Etsy", "Shopify", "TikTok", "其他"),
            c("white", "white", "white", "black")  # 字体颜色
          )
        )
    }
    
    # # 根据需要动态添加更多样式
    # if ("订单状态" %in% column_names) {
    #   table <- table %>%
    #     formatStyle(
    #       "订单状态",
    #       backgroundColor = styleEqual(
    #         c("处理中", "已完成", "取消"),
    #         c("yellow", "green", "red")
    #       ),
    #       color = styleEqual(
    #         c("处理中", "已完成", "取消"),
    #         c("black", "white", "white")
    #       )
    #     )
    # }
    
    table
  })
  
  # 返回选中行的索引
  reactive({
    input$order_table_rows_selected
  })
}
