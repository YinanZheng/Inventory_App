plan(multisession)

ui <- shinymanager::secure_app(
  theme = shinytheme("flatly"),
  tags_top = tags$div(
    tags$h2("ERP系统（国内端）登录", style = "text-align: center; color: #007BFF; font-weight: bold;"),
    tags$p("请输入您的用户名和密码以访问系统", style = "text-align: center; color: #666;")
  ),
  tags_bottom = tags$div(
    tags$p("© 2025 GoldenBean LLC", style = "text-align: center; color: #999;")
  ),
  ui = tagList(
    uiOutput("auth_status_ui"),  # 认证状态
    conditionalPanel(
      condition = "output.authenticated == true",
      shinycssloaders::withSpinner(
        uiOutput("dynamic_ui_async"),  # 异步渲染的 UI
        type = 6,
        color = "#007BFF",
        size = 1.5
      )
    )
  )
)
