uniqueItemsTableServer <- function(input, output, session, column_mapping, selection = "single", data, 
                                   options = table_default_options) {
  output$unique_items_table <- renderDT({
    # 初始化渲染表
    datatable_and_names <- render_table_with_images(
      data = data(),                 # 使用传递的 reactive 数据源
      column_mapping = column_mapping, # 映射用户友好的列名
      selection = selection, 
      image_column = "ItemImagePath",
      options = options
    )
    table <- apply_dynamic_styles(datatable_and_names$datatable, datatable_and_names$column_names)
    table
  }, server = TRUE)
  
  # 监听用户点击图片列
  observeEvent(input$unique_items_table_cell_clicked, {
    info <- input$unique_items_table_cell_clicked

    # if (info$row > nrow(data()) || info$row <= 0) {
    #   showNotification("无效的行索引！", type = "error")
    #   return()
    # }
    # 
    if (!"ItemImagePath" %in% colnames(data())) {
      showNotification("图片路径列不存在！", type = "error")
      return()
    }

    # 检查是否点击了图片列（第三列）
    if (!is.null(info) && !is.null(info$col) && !is.null(info$row)) {
      if (info$col == 2) {  # 第三列在 R 中的索引是 2
        img_path <- data()[info$row, "ItemImagePath"]

        # 确保图片路径为非空字符向量
        if (is.null(img_path) || is.na(img_path) || !is.character(img_path) || img_path == "") {
          showNotification("无效的图片路径！", type = "error")
          return()
        }

        img_host_path <- paste0(host_url, "/images/", basename(img_path))

        # 弹出窗口显示大图
        showModal(modalDialog(
          title = "订单图片预览",
          img(src = img_host_path, height = "500px", style = "display: block; margin: 0 auto;"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("关闭")
        ))
      }
    }
  })
  
  # 返回选中行的索引
  reactive({
    input$unique_items_table_rows_selected
  })
}
