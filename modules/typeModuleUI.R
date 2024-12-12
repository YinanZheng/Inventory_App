typeModuleUI <- function(id) {
  ns <- NS(id)  # 为模块创建命名空间
  tagList(
    fluidRow(
      column(5, uiOutput(ns("major_type_ui"))),  # 大类下拉框
      column(1, 
             div(style = "display: flex; justify-content: center; align-items: center; height: 100%;", 
                 actionButton(ns("add_major_type_btn"), label = NULL, icon = icon("plus"),
                              style = "font-size: 14px; width: 100%; max-width: 40px; height: 34px; padding: 0px; margin-top: 25px;")
             )
      ),
      column(5, uiOutput(ns("minor_type_ui"))),  # 小类下拉框
      column(1, 
             div(style = "display: flex; justify-content: center; align-items: center; height: 100%;", 
                 actionButton(ns("add_minor_type_btn"), label = NULL, icon = icon("plus"),
                              style = "font-size: 14px; width: 100%; max-width: 40px; height: 34px; padding: 0px; margin-top: 25px;")
             )
      )
    )
  )
}
