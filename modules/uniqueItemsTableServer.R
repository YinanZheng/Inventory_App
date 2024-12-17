uniqueItemsTableServer <- function(input, output, session, column_mapping, selection = "single", data) {
  output$unique_items_table <- renderDT({
    # Render table with images
    render_table_with_images(
      data = data(),                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection, 
      image_column = "ItemImagePath"
    ) %>%
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
      ) %>%
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
  })
  
  # 返回选中行的索引
  reactive({
    input$unique_items_table_rows_selected
  })
}