# 定义供应商模块
supplier_module <- function(input, output, session, maker_sheet_id) {
  # Reactive: 供应商数据
  maker_list <- reactive(read_sheet(maker_sheet_id))
  
  # 更新供应商下拉选项函数
  update_maker_choices <- function(maker_data) {
    sorted_data <- maker_data[order(maker_data$Pinyin), ]
    choices <- setNames(sorted_data$Maker, paste0(sorted_data$Maker, "(", sorted_data$Pinyin, ")"))
    updateSelectizeInput(session, "new_maker", choices = choices, server = TRUE)
  }
  
  # 初始化供应商选择器
  observe({
    update_maker_choices(maker_list())
  })
  
  # 添加供应商弹窗
  observeEvent(input$add_supplier_btn, {
    showModal(modalDialog(
      title = "添加新供应商",
      textInput("new_supplier_name", "请输入新供应商名称"),
      uiOutput("matched_suppliers"),
      footer = tagList(
        modalButton("取消"),
        actionButton("submit_supplier", "提交", class = "btn-primary")
      )
    ))
  })
  
  # 动态匹配供应商名称
  observe({
    output$matched_suppliers <- renderUI({
      current_name <- input$new_supplier_name
      
      if (is.null(current_name) || current_name == "") return(NULL)
      
      all_suppliers <- maker_list()$Maker
      matched <- all_suppliers[grepl(current_name, all_suppliers, ignore.case = TRUE)]
      
      if (length(matched) > 0) {
        tagList(
          h5("已存在的供应商如下，请勿重复加入:"),
          tags$ul(lapply(matched, tags$li))
        )
      } else {
        h5("该供应商可以加入！")
      }
    })
  })
  
  # 提交新供应商
  observeEvent(input$submit_supplier, {
    new_supplier <- input$new_supplier_name
    existing_suppliers <- maker_list()$Maker
    
    if (new_supplier %in% existing_suppliers) {
      showModal(modalDialog(
        title = "错误",
        paste0("供应商 '", new_supplier, "' 已经存在，请勿重复添加。"),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # 自动生成拼音
    pinyin_name <- remove_tone(stri_trans_general(new_supplier, "Latin"))
    
    if (new_supplier != "") {
      # 添加新供应商到 Google Sheets
      sheet_data <- data.frame(Maker = new_supplier, Pinyin = pinyin_name)
      sheet_append(maker_sheet_id, sheet_data)
      
      # 更新数据并刷新 UI
      new_data <- read_sheet(maker_sheet_id)
      update_maker_choices(new_data)
      
      show_custom_notification("新供应商添加成功！", type = "message")
      removeModal()
    } else {
      showModal(modalDialog(
        title = "错误",
        "供应商名称不能为空。",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
}