# Define UI
ui <- navbarPage(
  useShinyjs(),
  title = "库存管理系统（国内端）",
  theme = shinytheme("flatly"), # 可选主题
  
  # 全局脚本插入位置
  tags$head(
    tags$script(HTML("
    $(document).on('paste', '#paste_area', function(event) {
      const items = (event.originalEvent.clipboardData || event.clipboardData).items;
      for (let i = 0; i < items.length; i++) {
        if (items[i].type.indexOf('image') !== -1) {
          const file = items[i].getAsFile();
          const reader = new FileReader();
          reader.onload = function(evt) {
            Shiny.setInputValue('pasted_image', evt.target.result, {priority: 'event'});
          };
          reader.readAsDataURL(file);
          break;
        }
      }
    });
  "))
  ),
  
  tabPanel(
    "采购登记",
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
                                  style = "font-size: 14px; width: 100%; height: 32px; padding: 0px; margin-top: 27px;")
                 )
          )
        ),
        
        typeModuleUI("type_module"),
        
        fluidRow(
          column(12, textInput("new_name", "商品名:"))
        ),
        fluidRow(
          column(4, numericInput("new_quantity", "数量:", value = 0, min = 0, step = 1)),
          column(4, numericInput("new_product_cost", "成本:", value = 0, min = 0)),
          column(4, numericInput("new_shipping_cost", "运费", value = 0, min = 0))
        ),
        fluidRow(
          column(9,textInput("new_sku", "SKU(自动生成):", value = "")),
          column(3,actionButton("reset_btn", "清空输入", icon = icon("snowplow"), class = "btn-danger", 
                                style = "font-size: 14px; width: 100%; height: 42px; padding: 0px; margin-top: 27px;"))
        ),
        
        # 商品图片和粘贴区域
        tags$div(
          class = "card",
          style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px; margin-bottom: 15px;",
          tags$h5("商品图片上传", style = "margin-bottom: 15px; font-weight: bold; color: #007BFF;"),
          tags$div(
            id = "paste_area",
            style = "border: 2px dashed #ccc; padding: 20px; text-align: center; margin-bottom: 15px; position: relative;",
            div(id = "paste_prompt", "将商品截图粘贴到这里（Ctrl+V 或 Cmd+V）", style = "color: #888; font-size: 16px; font-style: italic;"),
            uiOutput("pasted_image_preview")
          ),
          fileInput("new_item_image", "或拖拽/选择商品图片上传:", accept = c("image/png", "image/jpeg"))
        ),
        
        # 进度条
        tags$div(
          id = "upload_progress",
          style = "display: none; margin-top: 15px;",
          tags$div(
            class = "progress",
            style = "height: 20px;",
            tags$div(class = "progress-bar progress-bar-striped progress-bar-animated", role = "progressbar", style = "width: 0%;", id = "progress_bar")
          )
        ),
        
        fluidRow(
          column(6, style = "text-align: left;", actionButton("add_btn", "添加/更新商品信息", icon = icon("pen"), style = "background-color: #006400; color: white;")),
          column(6, style = "text-align: right;", actionButton("update_image_btn", "更新商品图片", icon = icon("pen"), style = "background-color: #006400; color: white;"))
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow(
          column(12, checkboxInput("repeat_barcode", "重复条形码 (按商品数量)", value = TRUE))
        ),
        tags$div(
          class = "card",
          style = "padding: 15px; margin-bottom: 20px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
          # tags$h4("条形码生成"),
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            actionButton("export_single_btn", "生成当前SKU条形码", icon = icon("barcode"), class = "btn-info"),
            downloadButton("download_single_pdf", "下载条形码")
          ),
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            actionButton("export_batch_btn", "批量生成已添加商品条形码", icon = icon("barcode"), class = "btn-info"),
            downloadButton("download_batch_pdf", "下载批量条形码")
          )
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow(
          column(12, actionButton("confirm_btn", "登记采购货品", icon = icon("check"), class = "btn-primary", style = "width: 100%;"))
        )
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
          column(12, actionButton("toggle_item_table_purchase", "物品状态表（点击显示/隐藏）",
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")),  # 折叠按钮
          column(12, div(
            id = "item_table_container_purchase",  # 容器 ID
            style = "margin-bottom: 100px;",
            uniqueItemsTableUI("unique_items_table_purchase")
          ))
        )
      )
    )
  ), # end of 采购登记 tab
  
  tabPanel(
    "入库",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 卡片标题
              div(
                style = "margin-bottom: 10px; padding-bottom: 8px;",
                tags$h4("入库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              ),
              
              # SKU 输入框
              div(
                style = "margin-bottom: 15px;",
                textInput(
                  "inbound_sku", 
                  label = NULL, 
                  placeholder = "请扫描或输入条形码",
                  width = "100%"
                )
              ),
              
              # 瑕疵品复选框
              div(
                style = "margin-bottom: 20px; display: flex; align-items: center;",
                tags$input(
                  type = "checkbox", 
                  id = "defective_item", 
                  style = "width: 20px; height: 20px; margin-right: 10px;"
                ),
                tags$label("瑕疵品", `for` = "defective_item", style = "font-size: 18px; font-weight: bold; color: #444;")
              ),
              
              # 确认入库按钮
              actionButton(
                "confirm_inbound_btn", 
                "确认入库", 
                icon = icon("check"), 
                class = "btn-primary", 
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(12, uiOutput("inbound_item_info"), style = "margin-bottom: 40px;"), # 动态渲染物品信息
        ),
        
        tags$hr(), # 分隔线
        
        fluidRow(
          column(12, actionButton("toggle_item_table_inbound", "物品状态表（点击显示/隐藏）",
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")),  # 折叠按钮
          column(12, div(
            id = "item_table_container_inbound",  # 容器 ID
            style = "margin-bottom: 100px;",
            uniqueItemsTableUI("unique_items_table_inbound")
          ))
        )
      )
    )
  ), # end of 入库 tab
  
  tabPanel(
    "物品管理",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 卡片标题
              div(
                style = "margin-bottom: 10px; padding-bottom: 8px;",
                tags$h4("删除选中物品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              ),
              
              # 确认删除按钮
              actionButton(
                "confirm_delete_btn", 
                "确认删除", 
                icon = icon("check"), 
                class = "btn-primary", 
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(12, actionButton("toggle_item_table_manage", "物品状态表（点击显示/隐藏）",
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")),  # 折叠按钮
          column(12, div(
            id = "item_table_container_manage",  # 容器 ID
            style = "margin-bottom: 100px;",
            uniqueItemsTableUI("unique_items_table_manage")
          ))
        )
      )
    )
  ), # end of 物品管理 tab
  
  tabPanel(
    "瑕疵品管理",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(4, textInput("defect_sku", "瑕疵品登记:", placeholder = "请扫描条形码")),
          column(4, numericInput("defect_quantity", "数量:", value = 1, min = 1, step = 1)),
          column(4, actionButton(
            "defect_register", 
            "登记瑕疵品", 
            icon = icon("circle-exclamation"),
            class = "btn-warning", 
            style = "font-size: 14px; width: 100%; height: 42px; padding: 0px; margin-top: 27px;"
          ))
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow(
          column(4, textInput("repair_sku", "瑕疵品修复:", placeholder = "请扫描条形码")),
          column(4, numericInput("repair_quantity", "数量:", value = 1, min = 1, step = 1)),
          column(4, actionButton(
            "repair_register", 
            "登记修复品", 
            icon = icon("hammer"),
            class = "btn-success", 
            style = "font-size: 14px; width: 100%; height: 42px; padding: 0px; margin-top: 27px;"
          ))
        )
      ),
      
      mainPanel(
        fluidRow(
          column(12, actionButton("toggle_item_table_defect", "物品状态表（点击显示/隐藏）",
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")),  # 折叠按钮
          column(12, div(
            id = "item_table_container_defect",  # 容器 ID
            style = "margin-bottom: 100px;",
            uniqueItemsTableUI("unique_items_table_defect")
          ))
        )
      )
    )
  ), # end of 瑕疵品管理 tab
  
  
  tabPanel(
    "出库",
    sidebarLayout(
      sidebarPanel(
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("出库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("outbound_sku", NULL, placeholder = "请扫描或输入条形码"),
          actionButton(
            "confirm_outbound_btn", 
            "确认出库", 
            icon = icon("check"), 
            class = "btn-primary", 
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(12, uiOutput("outbound_item_info"), style = "margin-bottom: 40px;") # 动态渲染物品信息
        ),
        tags$hr(),
        fluidRow(
          column(12, actionButton("toggle_item_table_outbound", "物品状态表（点击显示/隐藏）",
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")), # 折叠按钮
          column(12, div(
            id = "item_table_container_outbound", # 容器 ID
            style = "margin-bottom: 100px;",
            uniqueItemsTableUI("unique_items_table_outbound")
          ))
        )
      )
    )
  ), # end of 出库 tab
  
  tabPanel(
    "售出",
    sidebarLayout(
      sidebarPanel(
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("售出操作", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
          textInput("sold_sku", NULL, placeholder = "请扫描或输入条形码"),
          actionButton(
            "confirm_sold_btn", 
            "确认售出", 
            icon = icon("check"), 
            class = "btn-success", 
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(12, uiOutput("sold_item_info"), style = "margin-bottom: 40px;") # 动态渲染物品信息
        ),
        tags$hr(),
        fluidRow(
          column(12, actionButton("toggle_item_table_sold", "物品状态表（点击显示/隐藏）",
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")), # 折叠按钮
          column(12, div(
            id = "item_table_container_sold", # 容器 ID
            style = "margin-bottom: 100px;",
            uniqueItemsTableUI("unique_items_table_sold")
          ))
        )
      )
    )
  ), # end of 出售 tab
  
  tabPanel(
    "查询",
    sidebarLayout(
      # 左侧输入区域
      sidebarPanel(
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请输入 SKU 或条形码", width = "100%")
        ),
        
        fluidRow(
          # 物品详情
          column(
            12,
            div(
              class = "card",
              style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              tags$h4("商品详情", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
              uiOutput("query_item_info") # 动态渲染物品信息
            )
          )
        )
      ),
      # 右侧显示区域
      mainPanel(
        fluidRow(
          # 库存状态图表
          column(
            6,
            div(
              class = "card",
              style = "margin-bottom: 20px; padding: 20px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              tags$h4("库存状态图表", style = "color: #28a745; font-weight: bold; margin-bottom: 15px;"),
              plotlyOutput("inventory_status_chart", height = "510px") # 使用 plotlyOutput
            )
          ),
          # 瑕疵情况图表
          column(
            6,
            div(
              class = "card",
              style = "margin-bottom: 20px; padding: 20px; border: 1px solid #dc3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
              tags$h4("瑕疵情况图表", style = "color: #dc3545; font-weight: bold; margin-bottom: 15px;"),
              plotlyOutput("defect_status_chart", height = "510px") # 使用 plotlyOutput
            )
          )
        ),
        
        fluidRow(
          column(12, actionButton("toggle_inventory_table", "库存表（点击显示/隐藏）", 
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")),  # 折叠按钮
          column(12, div(
            id = "inventory_table_container",  # 容器 ID
            DTOutput("filtered_inventory_table")
          ))
        )
      )
    )
  ), # end of 查询 tab
  
  tabPanel(
    "报表下载",
    sidebarLayout(
      sidebarPanel(
        
      ),
      mainPanel(
        
      )
    )
  ),
  
  # 添加全局底部
  footer = tags$div(
    style = "padding: 10px; background-color: #f8f9fa; text-align: center;",
    tags$span("版权所有 © 2024 Golden Bean LLC")
  )
)