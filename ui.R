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
    useShinyjs(),  # 启用 shinyjs
    # 加载动画独立放在顶层
    tags$div(
      id = "loading-screen",
      style = "position: fixed; width: 100%; height: 100%; background: white; 
               z-index: 9999; display: flex; flex-direction: column; 
               justify-content: center; align-items: center; text-align: center;",
      tags$img(src = "https://www.goldenbeanllc.com/icons/spinning_yarn.gif", 
               style = "width: 80px; height: 80px;"),
      tags$p("系统加载中，请稍后...", 
             style = "font-size: 18px; font-weight: bold; color: #333; margin-top: 10px;")
    ),
    uiOutput("dynamic_ui")  # 主 UI
  )
)