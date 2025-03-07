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
  
  ui = uiOutput("dynamic_ui")
)
