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
      if (!is.null(input$paste_area_pasted_image)) {
        shinyjs::show(ns("upload_progress"))
        update_progress(10)
        
        tryCatch({
          temp_path <- tempfile(fileext = ".jpg")
          base64enc::dataURI(input[[ns("paste_area_pasted_image")]], output = temp_path)
          update_progress(50)
          
          img <- magick::image_read(temp_path)
          img_info <- magick::image_info(img)
          
          # 图片预览
          output$pasted_image_preview <- renderUI({
            div(
              tags$img(src = input[[ns("paste_area_pasted_image")]], height = "200px", 
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
          showNotification("图片粘贴成功！", type = "message")
          
        }, error = function(e) {
          shinyjs::hide(ns("upload_progress"))
          showNotification("粘贴图片失败！", type = "error")
        })
      }
    })
    
    # 处理文件上传
    observeEvent(input$file_input, {
      if (!is.null(input$file_input)) {
        shinyjs::show(ns("upload_progress"))
        update_progress(10)
        
        tryCatch({
          file_data <- input$file_input
          uploaded_file(file_data)
          
          # 预览图片
          output$pasted_image_preview <- renderUI({
            img_data <- base64enc::dataURI(file_data$datapath, mime = "image/png")
            div(
              tags$img(src = img_data, height = "200px", 
                       style = "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 10px;"),
              tags$p(
                style = "color: #007BFF; font-size: 14px;",
                paste0("文件: ", file_data$name, ", 大小: ", round(file_data$size / 1024, 2), " KB")
              )
            )
          })
          
          update_progress(100)
          shinyjs::delay(500, shinyjs::hide(ns("upload_progress")))
          showNotification("图片上传成功！", type = "message")
          
        }, error = function(e) {
          shinyjs::hide(ns("upload_progress"))
          showNotification("上传图片失败！", type = "error")
        })
      }
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
