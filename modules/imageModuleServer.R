imageModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    uploaded_file <- reactiveVal(NULL)
    pasted_file <- reactiveVal(NULL)
    
    # 处理粘贴图片
    observeEvent(input$paste_area_pasted_image, {
      req(input$paste_area_pasted_image)  # 输入不能为空

      tryCatch({
        temp_path <- tempfile(fileext = ".jpg")

        # 清理 base64 数据
        img_data <- gsub("^data:image/[^;]+;base64,", "", input$paste_area_pasted_image)
        decoded_data <- base64enc::base64decode(img_data)  # 解码为 raw 数据

        # 写入临时文件
        writeBin(decoded_data, temp_path)

        # 读取图片信息
        img <- magick::image_read(temp_path)
        img_info <- magick::image_info(img)

        # 渲染图片预览
        output$pasted_image_preview <- renderUI({
          div(
            tags$img(src = input$paste_area_pasted_image, height = "200px",
                     style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 10px;"),
            tags$p(
              style = "color: #007BFF; font-size: 14px;",
              paste0("分辨率: ", img_info$width, "x", img_info$height,
                     ", 文件大小: ", round(file.size(temp_path) / 1024, 2), " KB")
            )
          )
        })

        pasted_file(list(datapath = temp_path, name = "pasted_image.jpg"))
        showNotification("图片粘贴成功！", type = "message", duration = 3)
      }, error = function(e) {
        output$pasted_image_preview <- renderUI({ NULL })
        showNotification(paste("粘贴图片失败！错误:", e$message), type = "error", duration = 5)
      })
    })
    
    
    # 处理文件上传
    observeEvent(input$file_input, {
      req(input$file_input)  # 确保输入存在
      
      tryCatch({
        file_data <- input$file_input
        
        # 确保文件路径存在
        if (!file.exists(file_data$datapath)) showNotification("文件路径无效", type = "error")
        
        # 获取 MIME 类型
        mime_type <- switch(
          tools::file_ext(file_data$name),
          "png" = "image/png",
          "jpeg" = "image/jpeg",
          "jpg" = "image/jpeg",
          showNotification("不支持的文件格式!", type = "error")
        )
        
        # 读取文件并手动生成 Base64 数据
        file_content <- readBin(file_data$datapath, "raw", file.info(file_data$datapath)$size)
        img_data <- paste0("data:", mime_type, ";base64,", base64enc::base64encode(file_content))
        
        # 渲染图片预览
        output$pasted_image_preview <- renderUI({
          div(
            tags$img(src = img_data, height = "200px",
                     style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 10px;"),
            tags$p(
              style = "color: #007BFF; font-size: 14px;",
              paste0("图片: ", file_data$name, ", 大小: ", round(file_data$size / 1024, 2), " KB")
            ),
            actionButton("clear_pasted_image", "清除图片", icon = icon("trash"), class = "btn-danger", style = "margin-top: 10px;")
          )
        })
        
        shinyjs::hide(ns("paste_prompt"))
        
        uploaded_file(file_data)
        showNotification("图片上传成功！", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("上传图片失败！错误:", e$message), type = "error", duration = 5)
      })
    })

    # 清除粘贴图片预览并恢复提示
    observeEvent(input$clear_pasted_image, {
      reset()
      showNotification("已清除粘贴的图片！", type = "message")
    })
    
    # 定义一个重置函数
    reset <- function() {
      shinyjs::reset(ns("file_input"))  # 使用命名空间重置 fileInput
      uploaded_file(NULL)  # 清空上传的文件
      pasted_file(NULL)    # 清空粘贴的文件
      output$pasted_image_preview <- renderUI({ NULL })  # 清空图片预览
      shinyjs::show(ns("paste_prompt"))  # 显示粘贴提示
    }
    
    showNotification(uploaded_file)
    
    return(list(
      uploaded_file = uploaded_file,
      pasted_file = pasted_file,
      reset = reset  # 返回重置函数
    ))
  })
}
