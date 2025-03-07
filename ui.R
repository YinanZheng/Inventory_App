# 自定义登录页面
custom_login <- function(input, output, session) {
  renderUI({
    fluidPage(
      theme = shinytheme("flatly"),
      tags$div(
        style = "padding: 20px; text-align: center;",
        tags$h2("ERP系统 (国内端) 登录", style = "color: #007BFF; font-weight: bold;"),
        tags$p("请输入您的用户名和密码以访问系统", style = "color: #666;"),
        textInput("user_name", "用户名:", width = "300px"),
        passwordInput("password", "密码:", width = "300px"),
        actionButton("login_btn", "登录", style = "background-color: #343a40; color: white; padding: 10px 20px;"),
        tags$div(style = "margin-top: 20px;", tags$p("© 2025 GoldenBean LLC", style = "color: #999;"))
      )
    )
  })
}

ui <- shinymanager::secure_app(
  theme = shinytheme("flatly"), # 与现有主题一致
  custom_login_page = custom_login,
  ui = uiOutput("dynamic_ui")
)
