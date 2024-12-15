uniqueItemsTableServer <- function(input, output, session, data) {
  output$unique_items_table <- renderDT({
    # Define column mapping for user-friendly display
    column_mapping <- list(
      SKU = "条形码",
      ItemName = "商品名",
      ItemImagePath = "商品图片",
      Maker = "供应商",
      MajorType = "大类",
      MinorType = "小类",
      Status = "库存状态",
      Defect = "物品状态"
    )
    
    # Render table with images
    render_table_with_images(
      data = data(),                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      image_column = "ItemImagePath"   # 指定图片列
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
          c("lightblue", "blue", "purple", "darkblue", "yellow", "brown", "red")
        ),
        color = styleEqual(
          c("采购", "国内入库", "国内售出", "国内出库", "美国入库", "美国售出", "退货"),
          c("black", "white", "white", "white", "black", "white", "white")
        )
      )
  })
}