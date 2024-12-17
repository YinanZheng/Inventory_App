imageModuleUI <- function(id, label = "图片上传") {
  ns <- NS(id)
  
  tagList(
    # 模块的主要 UI
    tags$div(
      class = "card",
      style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px; margin-bottom: 15px;",
      tags$h5(label, style = "margin-bottom: 15px; font-weight: bold; color: #007BFF;"),
      
      # 粘贴区域
      tags$div(
        id = ns("paste_area"), # 使用模块命名空间
        style = "border: 2px dashed #ccc; padding: 20px; text-align: center; margin-bottom: 15px; position: relative;",
        div(id = ns("paste_prompt"), "将图片粘贴到这里（Ctrl+V 或 Cmd+V）", 
            style = "color: #888; font-size: 16px; font-style: italic;"),
        uiOutput(ns("pasted_image_preview")) # 图片预览区域
      ),
      
      # 文件上传区域
      fileInput(ns("file_input"), "或选择图片上传:", accept = c("image/png", "image/jpeg")),
    )
  )
}
