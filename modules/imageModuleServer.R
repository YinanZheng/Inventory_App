imageModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    uploaded_file <- reactiveVal(NULL)
    pasted_file <- reactiveVal(NULL)
    
    # 更新进度条
    update_progress <- function(progress) {
      shinyjs::runjs(sprintf("$('#%s').css('width', '%d%%');", ns("progress_bar"), progress))
    }
    
    # 处理粘贴图片
    observeEvent(input$paste_area_pasted_image, {
      req(input$paste_area_pasted_image)  # 输入不能为空
      
      shinyjs::show(ns("upload_progress"))
      update_progress(10)
      
      tryCatch({
        temp_path <- tempfile(fileext = ".jpg")
        
        # 清理 base64 数据
        img_data <- gsub("^data:image/[^;]+;base64,", "", input$paste_area_pasted_image)
        decoded_data <- base64enc::base64decode(img_data)  # 解码为 raw 数据
        
        # 写入临时文件
        writeBin(decoded_data, temp_path)
        update_progress(50)
        
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
        update_progress(100)
        shinyjs::delay(500, shinyjs::hide(ns("upload_progress")))
        showNotification("图片粘贴成功！", type = "message", duration = 3)
      }, error = function(e) {
        shinyjs::hide(ns("upload_progress"))
        output$pasted_image_preview <- renderUI({ NULL })
        showNotification(paste("粘贴图片失败！错误:", e$message), type = "error", duration = 5)
      })
    })
    
    
    # 处理文件上传
    observeEvent(input$file_input, {
      req(input$file_input)  # 输入不能为空
      
      shinyjs::show(ns("upload_progress"))
      update_progress(10)
      
      tryCatch({
        file_data <- input$file_input
        mime_type <- ifelse(tools::file_ext(file_data$name) == "png", "image/png", "image/jpeg")
        
        # 转换为 base64
        img_data <- base64enc::dataURI(file_data$datapath, mime = mime_type)
        
        # 渲染图片预览
        output$pasted_image_preview <- renderUI({
          div(
            tags$img(src = img_data, height = "200px",
                     style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 10px;"),
            tags$p(
              style = "color: #007BFF; font-size: 14px;",
              paste0("文件: ", file_data$name, ", 大小: ", round(file_data$size / 1024, 2), " KB")
            )
          )
        })
        
        uploaded_file(file_data)
        update_progress(100)
        shinyjs::delay(500, shinyjs::hide(ns("upload_progress")))
        showNotification("图片上传成功！", type = "message", duration = 3)
      }, error = function(e) {
        shinyjs::hide(ns("upload_progress"))
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
