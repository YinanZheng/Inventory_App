ui <- shinymanager::secure_app(
  # 自定义登录页面的外观（可选）
  theme = shinytheme("flatly"), # 与现有主题一致
  tags_top = tags$div(
    tags$h2("ERP系统（国内端）登录", style = "text-align: center; color: #007BFF; font-weight: bold;"),
    tags$p("请输入您的用户名和密码以访问系统", style = "text-align: center; color: #666;")
  ),
  tags_bottom = tags$div(
    tags$p("© 2025 GoldenBean LLC", style = "text-align: center; color: #999;")
  ),
  
  ui = tagList(
    # 添加加载页面，默认显示
    div(
      id = "loading_page",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: #f5f5f5; z-index: 9999; display: flex; justify-content: center; align-items: center;",
      div(
        style = "text-align: center;",
        tags$h3("加载中，请稍候...", style = "color: #007BFF;"),
        # 简单的 CSS 加载动画
        tags$div(
          style = "width: 50px; height: 50px; border: 5px solid #007BFF; border-top: 5px solid transparent; border-radius: 50%; animation: spin 1s linear infinite;",
          tags$style(HTML("@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }"))
        )
      )
    ),
    # 主界面，默认隐藏
    div(
      id = "main_ui",
      style = "display: none;",
      uiOutput("dynamic_ui")
    )
  )
)