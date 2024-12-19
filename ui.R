# Define UI
ui <- navbarPage(
  title = "库存管理系统（国内端）",
  theme = shinytheme("flatly"), # 可选主题
  position = "fixed-top",
  
  header = tagList(
    shinyjs::useShinyjs(),  # 启用 shinyjs
    
    tags$head(
      tags$style(HTML("
      body {
        padding-top: 70px; /* 为导航栏腾出空间 */
      }
      
      /* Flexbox 容器 */
      .layout-container {
        display: flex; /* Flex 布局 */
        flex-wrap: nowrap; /* 禁止换行 */
        height: 100%; /* 满高布局 */
      }

      /* Sticky Sidebar */
      .sticky-sidebar {
        position: sticky; /* 保持固定 */
        top: 70px; /* 与导航栏对齐 */
        z-index: 900;
        width: 450px; /* 固定宽度 */
        height: calc(100vh - 70px); /* 自动计算高度 */
        overflow-y: auto; /* 滚动支持 */
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background-color: #f9f9f9;
        flex-shrink: 0; /* 防止压缩 */
      }
      
     /* Sticky info area */
      .sticky-info {
        position: sticky; 
        top: 70px; 
        z-index: 105;
        background-color: white; 
        padding: 10px; 
        border-bottom: 1px solid #ddd; 
        margin-bottom: 40px; 
        max-width: 100%;
        max-height: 450px; 
        overflow-x: hidden; /* 防止水平滚动 */
        overflow-y: auto; /* 垂直滚动 */
        box-shadow: 0 2px 4px rgba(0,0,0,0.1); /* 添加阴影以确保视觉分隔 */
      }
    
      /* 主面板 */
      .main-panel {
        flex-grow: 1; /* 占据剩余空间 */
        padding: 20px;
        padding-top: 0px;
        background-color: #ffffff;
      }

      /* 响应式布局 */
      @media (max-width: 768px) {
        .layout-container {
          flex-direction: column; /* 垂直排列 */
        }
        .sticky-sidebar {
          position: static; /* 不再固定 */
          width: 100%; /* 全宽 */
          height: auto; /* 高度自适应 */
        }
        .main-panel {
          margin-left: 0; /* 无偏移 */
        }
      }
    ")),
      
      tags$script(HTML("
        $(document).on('paste', '[id$=\"paste_area\"]', function(event) {
          const items = (event.originalEvent.clipboardData || event.clipboardData).items;
          for (let i = 0; i < items.length; i++) {
            if (items[i].type.indexOf('image') !== -1) {
              const file = items[i].getAsFile();
              const reader = new FileReader();
    
              reader.onload = function(evt) {
                // 使用 currentTarget 确保获取的是父级元素的 id
                const inputId = event.currentTarget.id + '_pasted_image';
                Shiny.setInputValue(inputId, evt.target.result, {priority: 'event'});
              };
    
              reader.readAsDataURL(file);
              break;
            }
          }
        });"))     
      
    )
  ),
  
  tabPanel(
    "采购登记", icon = icon("shopping-cart"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        fluidRow(
          column(10, 
                 selectizeInput("new_maker", "供应商:", choices = NULL, width = "100%",
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
          column(12, textInput("new_name", "商品名:", width = "100%"))
        ),
        fluidRow(
          column(4, numericInput("new_quantity", "数量:", value = 0, min = 0, step = 1)),
          column(4, numericInput("new_product_cost", "成本:", value = 0, min = 0)),
          column(4, numericInput("new_shipping_cost", "运费", value = 0, min = 0))
        ),
        fluidRow(
          column(9,textInput("new_sku", "SKU(自动生成):", value = "", width = "100%")),
          column(3,actionButton("reset_btn", "清空", icon = icon("snowplow"), class = "btn-danger", 
                                style = "font-size: 14px; width: 100%; height: 42px; padding: 0px; margin-top: 27px;"))
        ),
        
        fluidRow(
          column(3, 
                 tags$div(
                   tags$label("库存补录", class = "control-label"),  # 控件的标签
                   switchInput(
                     inputId = "inventory_switch",  # 开关 ID
                     label = NULL,                 # 开关本身不显示标签
                     value = FALSE,                # 默认关闭
                     size = "default"
                   )
                 )
          ),
          column(6, 
                 # 条件显示日期选择器
                 conditionalPanel(
                   condition = "input.inventory_switch == true",  # JavaScript 条件
                     dateInput(
                       inputId = "purchase_date",
                       label = "采购日期:",
                       value = Sys.Date(),  # 默认日期为今天
                       width = "100%"
                     )
                   )
                 )
        ),
        
        
        imageModuleUI("image_purchase"),
        
        fluidRow(
          column(12, style = "text-align: left;", actionButton("add_btn", "添加/更新采购货品信息", width = "100%", icon = icon("pen"), style = "background-color: #006400; color: white;")),
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow(
          column(12, actionButton("confirm_btn", "确认登记采购货品", icon = icon("check"), class = "btn-primary", style = "width: 100%;"))
        )
      ),
      
      div(
        class = "main-panel",
        div(
          class = "sticky-info",
          div(
            "已添加商品",
            style = "font-size: 18px; font-weight: bold; color: #333; background-color: #c3d8fa; 
             padding: 10px; text-align: center; border-radius: 4px;"
          ),
          column(12, DTOutput("added_items_table")),
          column(12, actionButton("delete_btn", "删除选中记录", icon = icon("trash"), class = "btn-danger")),
          
          tags$hr(), # 分隔线
          
          div(
            textOutput("total_cost"),
            style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;"
          )
        ),
        
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
    "入库", icon = icon("arrow-circle-down"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
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
                  placeholder = "请扫描或输入SKU",
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
              ),
              
              tags$hr(),
              
              # 条形码生成下载按钮
              tags$div(
                class = "card",
                style = "padding: 15px; margin-bottom: 20px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
                
                tags$div(
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  actionButton("export_select_btn", "生成选中商品条形码", icon = icon("barcode"), class = "btn-info"),
                  downloadButton("download_select_pdf", "下载条形码", class = "btn-info")
                )
              )
            )
          )
        )
      ),
      div(
        class = "main-panel",
        
        div(
          class = "sticky-info",  
          column(12, uiOutput("inbound_item_info"), style = "margin-bottom: 40px;") # 动态渲染物品信息
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
    "出库", icon = icon("arrow-circle-up"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("出库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("outbound_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          actionButton(
            "confirm_outbound_btn", 
            "确认出库", 
            icon = icon("check"), 
            class = "btn-primary", 
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          )
        )
      ),
      div(
        class = "main-panel",
        div(
          class = "sticky-info",
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
    "售出", icon = icon("dollar-sign"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("售出操作", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
          textInput("sold_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          actionButton(
            "confirm_sold_btn", 
            "确认售出", 
            icon = icon("check"), 
            class = "btn-success", 
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          )
        )
      ),
      div(
        class = "main-panel",
        div(
          class = "sticky-info",
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
  
  
  # tags$div(class = "navbar-divider", style = "height: 2px; margin: 0 15px;"),
  
  
  tabPanel(
    "物品管理", icon = icon("list-check"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
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
        ),
        
        tags$hr(), # 分隔线
        
        imageModuleUI("image_manage"),
        
        actionButton("update_image_btn", "更新商品图片", icon = icon("pen"), style = "background-color: #006400; color: white;")
        
      ),
      div(
        class = "main-panel",
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
    "瑕疵品管理", icon = icon("exclamation-circle"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        fluidRow(
          column(5, textInput("defect_sku", "瑕疵品登记:", placeholder = "请扫描或输入SKU")),
          column(3, numericInput("defect_quantity", "数量:", value = 1, min = 1, step = 1)),
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
          column(5, textInput("repair_sku", "瑕疵品修复:", placeholder = "请扫描或输入SKU")),
          column(3, numericInput("repair_quantity", "数量:", value = 1, min = 1, step = 1)),
          column(4, actionButton(
            "repair_register", 
            "登记修复品", 
            icon = icon("hammer"),
            class = "btn-success", 
            style = "font-size: 14px; width: 100%; height: 42px; padding: 0px; margin-top: 27px;"
          ))
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        tags$div(
          tags$label("仅显示瑕疵品", class = "control-label"),  
          switchInput(
            inputId = "show_defects_only",  # 开关 ID
            label = NULL,                  # 不显示标签在开关上
            value = FALSE                  # 默认值：关闭
          )
        )
      ),
      
      div(
        class = "main-panel",
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
    "查询", icon = icon("search"), 
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%")
        )
      ),
      div(
        class = "main-panel",
        div(
          class = "sticky-info",
          # 使用 tabsetPanel 来组织分页
          tabsetPanel(
            type = "tabs", # 使用 tabs 样式
            tabPanel(
              "库存状态",
              fluidRow(
                # 物品详情
                column(
                  6,
                  div(
                    class = "card",
                    style = "height: 350px; margin-bottom: 5px; padding: 5px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                    tags$h4("商品详情", style = "color: #007BFF; font-weight: bold; padding-left: 10px;"),
                    uiOutput("query_item_info") # 动态渲染物品信息
                  )
                ),
                
                column(
                  6,
                  div(
                    class = "card",
                    style = "margin-bottom: 5px; padding: 5px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                    tags$h4("库存状态图表", style = "color: #28a745; font-weight: bold; padding-left: 10px;"),
                    plotlyOutput("inventory_status_chart", height = "300px") # 使用 plotlyOutput
                  )
                )
              )
            ),
            tabPanel(
              "瑕疵情况",
              div(
                class = "card",
                style = "margin-bottom: 5px; padding: 5px; border: 1px solid #dc3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                tags$h4("瑕疵情况图表", style = "color: #dc3545; font-weight: bold; padding-left: 10px"),
                plotlyOutput("defect_status_chart", height = "300px") # 使用 plotlyOutput
              )
            )
            # 你可以在这里添加更多的 tabPanel 来扩展图表
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
    "数据下载", icon = icon("download"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        selectizeInput("maker", "选择供应商:", choices = NULL, selected = NULL, multiple = TRUE),
        actionButton("select_all", "全选", class = "btn-primary"),
        actionButton("clear_all", "清空", class = "btn-danger"),
        tags$hr(),
        
        selectInput("major_type", "选择大类:", choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("minor_type", "选择小类:", choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("unique_status", "选择状态:", choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("unique_defect", "选择瑕疵状态:", choices = NULL, selected = NULL, multiple = TRUE),
        
        dateRangeInput("purchase_time_range", "采购时间:", start = NULL, end = NULL),
        # dateRangeInput("entry_time_range", "国内入库时间:", start = NULL, end = NULL),
        # dateRangeInput("exit_time_range", "国内出库时间:", start = NULL, end = NULL),
        # dateRangeInput("sold_time_range", "国内售出时间:", start = NULL, end = NULL),
        
        actionButton("reset_filters", "重置筛选", class = "btn-secondary"),
        
        tags$hr(),
        
        downloadButton("download_unique_items_xlsx", "下载当前筛选结果 (Excel)", 
                       class = "btn-primary", style = "width: 100%;")      
      ),
      div(
        class = "main-panel",
        uniqueItemsTableUI("unique_items_table_download")
      )
    )
  ) # End of 数据下载 tab
  
  # # 添加全局底部
  # footer = tags$div(
  #   style = "padding: 10px; background-color: #f8f9fa; text-align: center;",
  #   tags$span("版权所有 © 2024 Golden Bean LLC")
  # )
)