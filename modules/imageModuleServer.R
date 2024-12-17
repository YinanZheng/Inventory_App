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
        
        # 解码并保存截图到临时文件夹
        base64_decode_image(input$paste_area_pasted_image, temp_path)
        
        # 读取图片信息
        img <- magick::image_read(temp_path)
        img_info <- magick::image_info(img)
        img_info$filesize <- file.size(temp_path)
        
        # 渲染图片预览
        output$pasted_image_preview <- render_image_preview(
          img_src = input$paste_area_pasted_image,
          img_info = img_info
        )

        pasted_file(list(datapath = temp_path, name = "pasted_image.jpg"))
        
        showNotification("图片粘贴成功！", type = "message", duration = 3)
      }, error = function(e) {
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
        
        uploaded_file(file_data)
        
        # 读取文件并手动生成 Base64 数据
        file_content <- readBin(file_data$datapath, "raw", file.info(file_data$datapath)$size)
        img_data <- paste0("data:", mime_type, ";base64,", base64enc::base64encode(file_content))
        
        # 读取图片信息
        img <- magick::image_read(file_data$datapath)
        img_info <- magick::image_info(img)
        img_info$filesize <- file.size(file_data$datapath)
        
        # 渲染图片预览
        output$pasted_image_preview <- render_image_preview(
          img_src = img_data,
          img_info = img_info
        )
        showNotification("图片上传成功！", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("上传图片失败！错误:", e$message), type = "error", duration = 5)
      })
    })

    
    # 定义一个重置函数
    reset <- function() {
      shinyjs::reset(ns("file_input"))  # 使用命名空间重置 fileInput
      uploaded_file(NULL)  # 清空上传的文件
      pasted_file(NULL)    # 清空粘贴的文件
      output$pasted_image_preview <- renderUI({ NULL })  # 清空图片预览
      shinyjs::show(ns("paste_prompt"))  # 显示粘贴提示
    }
    
    return(list(
      uploaded_file = uploaded_file,
      pasted_file = pasted_file,
      reset = reset  # 返回重置函数
    ))
  })
}
