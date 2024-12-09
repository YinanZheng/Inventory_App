# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("库存管理系统"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(10, 
               selectizeInput("new_maker", "供应商:", choices = NULL, 
                              options = list(placeholder = '输入供应商名称（或拼音）进行搜索', maxOptions = 500))
        ),
        column(2, 
               div(style = "display: flex; justify-content: flex-start; align-items: center; height: 100%;", 
                   actionButton("add_supplier_btn", label = NULL, icon = icon("plus"), 
                                style = "font-size: 14px; width: 100%; max-width: 70px; height: 34px; padding: 0px; margin-top: 25px;")
               )
        )
      ),
      fluidRow(
        column(6, uiOutput("major_type_ui")), ## 大类
        column(6, uiOutput("minor_type_ui")), ## 小类
      ),
      fluidRow(
        column(12, textInput("new_name", "商品名:"))
      ),
      fluidRow(
        column(4, numericInput("new_quantity", "数量:", value = 1, min = 1, step = 1)),
        column(4, numericInput("new_cost", "成本:", value = 0, min = 0, max = 999, step = 1)),
        column(4, numericInput("shipping_cost", "运费", value = 0, min = 0, step = 1))
      ),
      textInput("new_sku", "SKU(自动生成):", value = ""),
      fileInput("new_item_image", "商品图片:"),
      actionButton("reset_btn", "清空输入", icon = icon("undo"), class = "btn-danger"),
      
      tags$hr(), # 分隔线
      h4("入库操作"),
      actionButton("add_btn", "添加/更新商品信息"),
      actionButton("confirm_btn", "确认入库", class = "btn-primary"),
      
      tags$hr(), # 分隔线
      h4("条形码操作"),
      actionButton("export_btn", "生成条形码"),
      downloadButton("barcode_pdf", "下载条形码"),
    ),
    
    mainPanel(
      # 使用 fluidRow 分成上下两个部分
      fluidRow(
        column(12,  div(
          h4("符合条件的库存记录"), 
          style = "font-size: 20px; font-weight: bold; color: #333; background-color: #f9f9f9; 
             padding: 3px; border: 2px solid #ddd; border-radius: 3px; text-align: center;"
        )),
        column(12, DTOutput("filtered_inventory_table"))
      ),
      
      tags$hr(), # 分隔线
      fluidRow(
        column(12, div(
          h4("已添加商品"), 
          style = "font-size: 20px; font-weight: bold; color: #333; background-color: #f9f9f9; 
             padding: 3px; border: 2px solid #ddd; border-radius: 3px; text-align: center;"
        )),
        column(12, DTOutput("added_items_table")),
        column(12, actionButton("delete_btn", "删除选中记录", icon = icon("trash"))),
      ),
      
      tags$hr(), # 分隔线
      div(
        textOutput("total_cost"),
        style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;"
      ),
      
      tags$hr(), # 分隔线
      fluidRow(
        column(12, div(
          h4("物品状态追踪表"), 
          style = "font-size: 20px; font-weight: bold; color: #333; background-color: #f9f9f9; 
             padding: 3px; border: 2px solid #ddd; border-radius: 3px; text-align: center;"
        )),
        column(12, DTOutput("unique_items_data"))
      )
    )
  )
)