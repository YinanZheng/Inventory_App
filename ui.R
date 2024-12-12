# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("国内入库出库系统"),
  
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
      
      typeModuleUI("type_module"),
   
      fluidRow(
        column(12, textInput("new_name", "商品名:"))
      ),
      fluidRow(
        column(4, numericInput("new_quantity", "数量:", value = 1, min = 1, step = 1)),
        column(4, numericInput("new_product_cost", "成本:", value = 0, min = 0, max = 999, step = 1)),
        column(4, numericInput("new_shipping_cost", "运费", value = 0, min = 0, step = 1))
      ),
      fluidRow(
        column(9,textInput("new_sku", "SKU(自动生成):", value = "")),
        column(3,actionButton("reset_btn", "清空输入", icon = icon("snowplow"), class = "btn-danger", 
                              style = "margin-top: 25px; height: 34px; width: 100%;"))
      ),
      
      fileInput("new_item_image", "商品图片:", accept = c("image/png", "image/jpeg")),
      
      actionButton("add_btn", "添加/更新商品信息", icon = icon("pen"),
                   style = "background-color: #006400; color: white;"),
      
      tags$hr(), # 分隔线
      
      fluidRow(
        column(12, checkboxInput("repeat_barcode", "重复条形码 (按商品数量)", value = TRUE))
      ),
      tags$div(
        class = "card",
        style = "padding: 15px; margin-bottom: 20px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
        # tags$h4("条形码生成"),
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          actionButton("export_single_btn", "生成当前SKU条形码", icon = icon("barcode")),
          downloadButton("download_single_pdf", "下载条形码")
        ),
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          actionButton("export_batch_btn", "批量生成已添加商品条形码", icon = icon("barcode")),
          downloadButton("download_batch_pdf", "下载批量条形码")
        )
      ),
      
      tags$hr(style = "margin: 5px 0; border: none;"),
      
      fluidRow(
        column(12, actionButton("confirm_btn", "确认入库", icon = icon("check"), class = "btn-primary", style = "width: 100%;"))
      ),
   
      tags$hr(), # 分隔线
      
      fluidRow(
        column(4, textInput("defect_sku", "瑕疵品登记:", placeholder = "请扫描条形码")),
        column(4, numericInput("defect_quantity", "数量:", value = 1, min = 1, step = 1)),
        column(4, actionButton(
          "defect_register", 
          "登记", 
          icon = icon("circle-exclamation"),
          class = "btn-warning", 
          style = "margin-top: 25px; height: 34px; width: 100%;"
        ))
      ),
      
      tags$hr(style = "margin: 5px 0; border: none;"),
      
      fluidRow(
        column(4, textInput("repair_sku", "瑕疵品修复:", placeholder = "请扫描条形码")),
        column(4, numericInput("repair_quantity", "数量:", value = 1, min = 1, step = 1)),
        column(4, actionButton(
          "repair_register", 
          "登记", 
          icon = icon("hammer"),
          class = "btn-warning", 
          style = "margin-top: 25px; height: 34px; width: 100%;"
        ))
      ),
      
      tags$hr(), # 分隔线
      
      fluidRow(
        column(8, textInput("outbound_sku", "出库:", placeholder = "请扫描条形码")),
        column(4, actionButton(
          "undo_outbound_btn", 
          "撤回最近出库", 
          icon = icon("undo"),
          class = "btn-warning", 
          style = "margin-top: 25px; height: 34px; width: 100%;"
        ))
      ),
      
      tags$hr(style = "margin: 5px 0; border: none;"),
      
      fluidRow(
        column(8, textInput("sold_sku", "售出:", placeholder = "请扫描条形码")),
        column(4, actionButton(
          "undo_sold_btn", 
          "撤回最近售出", 
          icon = icon("undo"),
          class = "btn-warning", 
          style = "margin-top: 25px; height: 34px; width: 100%;"
        ))
      ),
      
      tags$hr(style = "margin: 5px 0; border: none;"),
      
      fluidRow(
        column(6, selectInput(
          "target_status", 
          "移库目标:", 
          choices = c(
            "国内入库", 
            "国内出库", 
            "国内售出", 
            "美国入库", 
            "美国售出"
          )
        )),
        column(6, actionButton(
          "move_selected", 
          "移库选中物品", 
          class = "btn-primary", 
          style = "margin-top: 25px; height: 34px; width: 100%;"
        ))
      ),
      
      tags$hr(), # 分隔线
      
      textInput("sku_inventory", "查询库存总览:", placeholder = "请输入SKU"),
      actionButton("refresh_inventory_btn", "查看", icon = icon("magnifying-glass")),
      br(), br(),
      uiOutput("inventory_overview_ui")  # 动态 UI
    ),
    
    mainPanel(
      fluidRow(
        column(12,   div(
          "已添加商品",
          style = "font-size: 18px; font-weight: bold; color: #333; background-color: #c3d8fa; 
             padding: 10px; text-align: center; border-radius: 4px;"
        )),
        column(12, DTOutput("added_items_table")),
        column(12, actionButton("delete_btn", "删除选中记录", icon = icon("trash"), class = "btn-danger")),
      ),
      tags$hr(), # 分隔线
      div(
        textOutput("total_cost"),
        style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;"
      ),
      
      tags$hr(), # 分隔线
      
      fluidRow(
        column(12, actionButton("toggle_inventory_table", "库存表（点击显示/隐藏）", 
                                style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa;")),  # 折叠按钮
        column(12, div(
          id = "inventory_table_container",  # 容器 ID
          style = "height: 300px; overflow-y: scroll;",  # 初始样式
          DTOutput("filtered_inventory_table")
        ))
      ),
      
      tags$hr(), # 分隔线
      
      fluidRow(
        column(12, actionButton("toggle_item_table", "物品状态表（点击显示/隐藏）",
                                style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa;")),  # 折叠按钮
        column(12, div(
          id = "item_table_container",  # 容器 ID
          style = "margin-bottom: 100px;",
          DTOutput("unique_items_table"),
        ))
      )
    )
  )
)