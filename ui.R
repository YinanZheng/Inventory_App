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
      
      /* 自定义 selectize 样式 */
      .custom-selectize .selectize-input {
        font-size: 12px !important; /* 设置输入框字体大小 */
      }
      .custom-selectize .selectize-dropdown-content {
        font-size: 12px !important; /* 设置下拉菜单字体大小 */
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
                                  style = "font-size: 14px; width: 100%; height: 34px; padding: 0px; margin-top: 26px;")
                 )
          )
        ),
        
        typeModuleUI("type_module"),
        
        fluidRow(
          column(8, selectizeInput(
            "new_name",                
            label = "商品名:",         
            choices = NULL,            
            options = list(
              placeholder = "请输入商品名...",
              create = TRUE            # 允许自定义输入值
            ),
            width = "100%"
          )),
          
          column(4, dateInput(
            inputId = "purchase_date",
            label = "采购日期:",
            value = Sys.Date(),  # 默认日期为今天
            width = "100%"
          ))
        ),
        fluidRow(
          column(4, numericInput("new_quantity", "数量:", value = 0, min = 0, step = 1)),
          column(4, numericInput("new_product_cost", "单价:", value = 0, min = 0)),
          column(4, numericInput("new_shipping_cost", "运费", value = 0, min = 0))
        ),
        fluidRow(
          column(9,textInput("new_sku", "SKU(自动生成):", value = "", width = "100%")),
          column(3,actionButton("reset_btn", "清空", icon = icon("snowplow"), class = "btn-danger", 
                                style = "font-size: 14px; width: 100%; height: 45px; padding: 0px; margin-top: 26px;"))
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
              
              div(
                id = "defective_notes_container",
                style = "display: none; margin-top: 10px;",
                textAreaInput(
                  inputId = "defective_notes",
                  label = "瑕疵品备注：",
                  placeholder = "请输入备注内容...",
                  width = "100%"
                )
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
        ),
          
        tags$hr(), # 分隔线
        
        fluidRow( 
          # 条形码生成下载按钮
          column(12,              
                 tags$div(
                   class = "card",
                   style = "padding: 15px; margin-bottom: 20px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
                   
                   # 卡片标题
                   div(
                     style = "margin-bottom: 10px; padding-bottom: 8px;",
                     tags$h4("条形码下载", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
                   ),
                   
                   tags$div(
                     style = "display: flex; justify-content: space-between; align-items: center;",
                     actionButton("export_select_btn", "生成选中商品条形码", icon = icon("barcode"), class = "btn-info"),
                     downloadButton("download_select_pdf", "下载条形码", class = "btn-info")
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
          radioButtons(
            inputId = "outbound_shipping_method",
            label = "选择国际运输方式:",
            choices = list("空运" = "空运", "海运" = "海运"),
            selected = "空运",  # 默认选择空运
            inline = TRUE       # 设置为横向排布
          ),
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
      
      # 左侧订单信息录入
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          # 订单录入表单
          tags$h4("物品筛选", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
          
          fluidRow(
            column(6, 
                   selectizeInput("sold_maker", "供应商:", choices = NULL, width = "100%",
                                  options = list(placeholder = '供应商名称(或拼音)...', maxOptions = 500)),
                   class = "custom-selectize" # 自定义 class
            ),
            column(6, selectizeInput(
              "sold_name",                
              label = "商品名:",         
              choices = NULL,            
              options = list(
                placeholder = "商品名...",
                create = TRUE            # 允许自定义输入值
              ),
              width = "100%"
            ),class = "custom-selectize" # 自定义 class
            )
          ),
          
          fluidRow(
            column(9, textInput("sold_sku", "输入或扫描条形码", placeholder = "请输入条形码", width = "100%")),
            column(3, actionButton("sold_reset_btn", "清空", icon = icon("snowplow"), class = "btn-danger", 
                                  style = "font-size: 14px; width: 100%; height: 45px; padding: 0px; margin-top: 26px;"))          
          )
        ),

        
        
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          # 订单录入表单
          tags$h4("订单登记", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
          textInput("order_id", "订单号", placeholder = "请输入订单号", width = "100%"),
          textInput("tracking_number1", "运单号", placeholder = "请输入运单号", width = "100%"),
          imageModuleUI("image_sold", label = "订单图片上传", label_color = "#28A745"),
          textAreaInput("order_notes", "订单备注", placeholder = "请输入备注内容", width = "100%"),
          actionButton(
            "register_order_btn",
            "登记/更新订单",
            icon = icon("save"),
            class = "btn-primary",
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          ),
        ),
        
        # 国际运输方式选择
        radioButtons(
          inputId = "sold_shipping_method",
          label = "选择国际运输方式:",
          choices = list("空运" = "空运", "海运" = "海运"),
          selected = "空运",  # 默认选择空运
          inline = TRUE       # 设置为横向排布
        ),
        
        actionButton(
          "confirm_order_btn", 
          "确认售出", 
          icon = icon("check"), 
          class = "btn-success", 
          style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
        )
      ),
      
      # 主面板：右侧物品选择和已选物品列表
      div(
        class = "main-panel",
        
        fluidRow(
          # 货架部分
          column(6,
                 div(
                   class = "card",
                   style = "padding: 20px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                   tags$h4(
                     HTML(paste0(as.character(icon("warehouse")), "  货架")), 
                     style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"
                   ),
                   DTOutput("shelf_table")  # 显示货架上的物品
                 )
          ),
          
          # 箱子部分
          column(6,
                 div(
                   class = "card",
                   style = "padding: 20px; margin-bottom: 20px; border: 1px solid #28A745; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                   tags$h4(
                     HTML(paste0(as.character(icon("box")), "  发货箱")), 
                     style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"
                   ),
                   DTOutput("box_table"),  # 显示已放入箱子的物品
                   actionButton("clear_selected_items", "清空发货箱", class = "btn-warning", style = "margin-top: 10px; width: 100%;")
                 )
          )
        ),
        
        fluidRow(
          column(12, actionButton("toggle_item_table_sold", "物品状态表（点击显示/隐藏）",
                                  style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")),  # 折叠按钮
          column(12, div(
            id = "item_table_container_sold",  # 容器 ID
            style = "margin-bottom: 100px;",
            uniqueItemsTableUI("unique_items_table_sold")
          ))
        )
      )
    )
  ), # end of 售出 tab
  
  
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
        
        div(
          class = "card shadow-sm", # 添加卡片样式
          style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
          # 卡片标题
          div(
            style = "margin-bottom: 10px; padding-bottom: 8px;",
            tags$h4("更新商品图片", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
  
          imageModuleUI("image_manage", label = ""),
          
          actionButton("update_image_btn", "更新商品图片", icon = icon("pen"), style = "background-color: #006400; color: white;")
          ),
        )
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
        
        # 登记瑕疵品部分
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            "register_defective", 
            "登记瑕疵品", 
            icon = icon("circle-exclamation"),
            class = "btn-warning", 
            style = "font-size: 16px; width: 100%;"
          )
        ),
        div(
          style = "margin-bottom: 20px;",
          tags$label("仅显示无瑕品", class = "control-label", style = "font-size: 12px; display: block; text-align: center;"),  
          switchInput(
            inputId = "show_perfects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        # 登记修复品部分
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            "register_repair", 
            "登记修复品", 
            icon = icon("hammer"),
            class = "btn-success", 
            style = "font-size: 16px; width: 100%;"
          )
        ),
        div(
          style = "margin-bottom: 20px;",
          tags$label("仅显示瑕疵品", class = "control-label", style = "font-size: 12px; display: block; text-align: center;"),  
          switchInput(
            inputId = "show_defects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        # 备注输入框
        textAreaInput(
          inputId = "manage_defective_notes",
          label = "备注：",
          placeholder = "请输入备注内容...",
          width = "100%"
        )
      ),
      
      # 主面板：物品状态表
      div(
        class = "main-panel",
        fluidRow(
          column(
            12, 
            actionButton(
              "toggle_item_table_defect", 
              "物品状态表（点击显示/隐藏）",
              style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;"
            )
          ),
          column(
            12, 
            div(
              id = "item_table_container_defect",  # 容器 ID
              style = "margin-bottom: 100px;",
              uniqueItemsTableUI("unique_items_table_defect")
            )
          )
        )
      )
    )
  ), # end of 瑕疵品管理 tab
  
  
  tabPanel(
    "国际物流管理", icon = icon("globe"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        fluidRow(
          column(
            12,
            textInput(
              "intl_tracking_number",
              "国际运单号:",
              placeholder = "请输入空运或海运运单号",
              width = "100%"
            )
          ),
          column(
            12,
            selectInput(
              "intl_shipping_method",
              "国际运输方式:",
              choices = c("空运" = "空运", "海运" = "海运"),
              selected = "空运",
              width = "100%"
            )
          ),
          column(
            6,
            actionButton(
              "link_tracking_btn",
              "挂靠运单",
              icon = icon("link"),
              class = "btn-primary",
              style = "margin-top: 20px; width: 100%;"
            )
          ),
          column(
            6,
            actionButton(
              "delete_tracking_btn",
              "删除运单",
              icon = icon("trash"),
              class = "btn-danger",
              style = "margin-top: 20px; width: 100%;"
            )
          )
        )
      ),
      div(
        class = "main-panel",
        uniqueItemsTableUI("unique_items_table_logistics")
      )
    )
  ), # end of 国际物流管理 tab
  
  
  
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
          # 使用 tabsetPanel 来组织分页
          tabsetPanel(
            type = "tabs", # 使用 tabs 样式
            tabPanel(
              "商品状态",
              fluidRow(
                column(
                  4,
                  div(
                    class = "card",
                    style = "height: 453.89px; margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                    tags$h4("商品信息", style = "color: #007BFF; font-weight: bold; padding-left: 10px;"),
                    uiOutput("query_item_info") # 动态渲染物品信息
                  )
                ),
                
                column(
                  4,
                  div(
                    class = "card",
                    style = "margin-bottom: 5px; padding: 5px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                    tags$h4("库存状态图表", style = "color: #28a745; font-weight: bold; padding-left: 10px;"),
                    plotlyOutput("inventory_status_chart", height = "400px") # 使用 plotlyOutput
                  )
                ),
                
                column(
                  4,
                  div(
                    class = "card",
                    style = "margin-bottom: 5px; padding: 5px; border: 1px solid #dc3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                    tags$h4("瑕疵情况图表", style = "color: #dc3545; font-weight: bold; padding-left: 10px"),
                    plotlyOutput("defect_status_chart", height = "400px") # 使用 plotlyOutput
                  )
                ),

                column(12, actionButton("toggle_inventory_table", "库存表（点击显示/隐藏）", 
                                        style = "font-weight: bold; width: 100%; font-size: 18px; background-color: #c3d8fa; color: black;")),  # 折叠按钮
                column(12, div(
                  id = "inventory_table_container",  # 容器 ID
                  DTOutput("filtered_inventory_table")
                ))
                
              )
            ), # end of 商品状态
            
            tabPanel(
              "采购开销",
              fluidRow(
                column(
                  12,
                  div(
                    class = "card",
                    style = "margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                    
                    # 选择器行
                    fluidRow(
                      column(4,                   
                             dateRangeInput(
                               "time_range",
                               label = "选择采购时间范围",
                               start = Sys.Date() - 30, # 默认最近30天
                               end = Sys.Date()
                             )),
                      column(4,
                             radioButtons(
                               "precision",
                               label = "选择统计精度",
                               choices = c("天" = "天", "周" = "周", "月" = "月", "年" = "年"),
                               selected = "天",
                               inline = TRUE # 使选项横向排列
                             )),
                      column(4,
                             radioButtons(
                               "expense_type",
                               label = "选择显示内容",
                               choices = c("总开销" = "total", "物品成本" = "cost", "运费开销" = "shipping"),
                               selected = "total",
                               inline = TRUE # 使选项横向排列
                             ))
                    ),
                    
                    # 图表行：柱状图 + 饼图
                    fluidRow(
                      column(9, plotlyOutput("bar_chart", height = "350px")), # 80% 宽度柱状图
                      column(3, plotlyOutput("pie_chart", height = "350px"))  # 20% 宽度饼图
                    )
                  )
                )
              )
            ) # end of 开销汇总tab
            
            
            # 你可以在这里添加更多的 tabPanel 来扩展图表
            
          ) #end of tabpanel
      )
    )
  ), # end of 查询 tab
  
  tabPanel(
    "数据下载", icon = icon("download"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        div(
          class = "card shadow-sm", # 添加卡片样式
          style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
          
          tags$h4("表格筛选", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          # 供应商筛选
          selectizeInput(
            inputId = "download_maker",
            label = "选择供应商:",
            choices = NULL,          # 动态加载供应商
            selected = NULL,         # 默认全选
            multiple = TRUE,         # 支持多选
            options = list(          # 提供更好的交互体验
              placeholder = "请选择供应商...",
              allowEmptyOption = TRUE,
              maxItems = 10          # 限制最多选中10个
            ),
            width = "100%"
          ),
          
          # 商品名称筛选
          selectizeInput(
            inputId = "download_item_name",
            label = "商品名称:",
            choices = NULL,          # 动态加载商品名称
            selected = NULL,         # 默认全选
            multiple = FALSE,        # 单选，适合精确匹配
            options = list(          # 提供更好的交互体验
              placeholder = "请输入商品名称...",
              create = FALSE         # 不允许用户输入新值
            ),
            width = "100%"
          ),
          
          # 采购日期筛选
          dateRangeInput(
            inputId = "download_date_range",
            label = "选择采购日期范围:",
            start = Sys.Date() - 30, # 默认最近30天
            end = Sys.Date(),        # 默认结束日期为今天
            format = "yyyy-mm-dd",   # 日期格式
            separator = " 至 ",
            width = "100%"
          ),
          
          actionButton("download_reset_filters", "重置筛选", class = "btn-secondary")
        ),
        
        tags$hr(),
        
        downloadButton("download_unique_items_xlsx", "下载物品明细表 (Excel)", 
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