uniqueItemsTableServer <- function(input, output, session, column_mapping, selection = "single", data) {
  output$unique_items_table <- renderDT({
    # 初始化渲染表
    datatable_and_names <- render_table_with_images(
      data = data(),                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection, 
      image_column = "ItemImagePath"
    )
    
    # 获取数据列名
    column_names <- table_and_names$column_names
    
    table <- table_and_names$datatable
    
    # 动态应用样式
    if ("库存状态" %in% column_names) {
      table <- table %>%
        formatStyle(
          "库存状态",
          backgroundColor = styleEqual(
            c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货"),
            c("lightgray", "#c7e89b", "darkgray", "#46a80d", "#173b02", "darkgray", "red")
          ),
          color = styleEqual(
            c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货"),
            c("black", "black", "black", "white", "white", "black", "white")
          )
        )
    }
    
    if ("物品状态" %in% column_names) {
      table <- table %>%
        formatStyle(
          "物品状态",
          backgroundColor = styleEqual(
            c("未知", "无瑕", "瑕疵", "修复"),
            c("darkgray", "green", "red", "orange")
          ),
          color = styleEqual(
            c("未知", "无瑕", "瑕疵", "修复"),
            c("black", "white", "white", "white")
          )
        )
    }
    
    if ("国际运输" %in% column_names) {
      table <- table %>%
        formatStyle(
          "国际运输",
          backgroundColor = styleEqual(
            c("空运", "海运"),
            c("lightblue", "darkblue")
          ),
          color = styleEqual(
            c("空运", "海运"),
            c("black", "white")
          )
        )
    }
    
    table
  })
  
  # 返回选中行的索引
  reactive({
    input$unique_items_table_rows_selected
  })
}
