# Define UI
ui <- navbarPage(
  title = "库存管理系统（国内端）",
  id = "inventory_china",  # 设置 ID，用于监听当前选中的主页面
  theme = shinytheme("flatly"), # 可选主题
  position = "fixed-top",
  
  header = tagList(
    shinyjs::useShinyjs(),  # 启用 shinyjs
    
    tags$head(
      tags$style(HTML("
      
      /* 默认显示导航栏标题 */
      .navbar-brand {
        display: inline-block !important;
      }
  
      /* 强制导航栏水平滚动，禁止换行 */
      .navbar-nav {
        display: flex !important; /* 使用 Flex 布局 */
        flex-wrap: nowrap !important; /* 禁止换行 */
        overflow-x: auto !important; /* 启用水平滚动 */
        white-space: nowrap !important; /* 确保内容不换行 */
      }
    
      /* 美化滚动条 */
      .navbar-nav::-webkit-scrollbar {
        height: 6px; /* 滚动条高度 */
      }
      .navbar-nav::-webkit-scrollbar-thumb {
        background: #007BFF; /* 滚动条颜色 */
        border-radius: 10px;
      }
    
      /* 禁止导航栏高度扩展 */
      .navbar {
        white-space: nowrap !important; /* 确保所有子元素在单行内 */
      }
    
     /* 当屏幕宽度小于 1380px 时，隐藏标题 */
      @media (max-width: 1380px) {
        .navbar-brand {
          display: none !important;
        }
      }
    
      /* 当屏幕宽度小于 768px 时，调整导航项的字体和间距 */
      @media (max-width: 768px) {
        .navbar-nav > li > a {
          font-size: 12px !important; /* 调整字体大小适配小屏幕 */
          padding: 6px 8px !important; /* 减少间距 */
        }
      }
    
    
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
        width: 380px; /* 固定宽度 */
        height: calc(100vh - 70px); /* 自动计算高度 */
        overflow-y: auto; /* 滚动支持 */
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background-color: #f9f9f9;
        flex-shrink: 0; /* 防止压缩 */
      }
    
      /* 主面板 */
      .main-panel {
        flex-grow: 1; /* 占据剩余空间 */
        padding: 20px;
        padding-top: 0px;
        background-color: #ffffff;
      }
    
      table.dataTable thead th {
        white-space: nowrap; /* 表头内容强制不换行 */
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
    "采购", icon = icon("shopping-cart"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "purchase_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = FALSE),
        
        tags$hr(),
        
        fluidRow(
          column(10, 
                 selectizeInput("new_maker", "供应商:", choices = NULL, width = "100%",
                                options = list(placeholder = '输入名称（或拼音）进行搜索', maxOptions = 500))
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
          column(
            7,
            autocompleteInputUI("purchase", label = "商品名：", placeholder = "请输入商品名...")
          ),  
          column(5, dateInput(
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
          column(12,textInput("new_sku", "SKU(自动生成):", value = "", width = "100%"))
        ),
        
        imageModuleUI("image_purchase"),
        
        actionButton("reset_btn", "重置采购登记", icon = icon("snowplow"), class = "btn-danger", 
                     style = "font-size: 14px; width: 100%; height: 45px; padding: 0px; margin-top: 26px;")
      ),
      
      div(
        class = "main-panel",
        style = "display: flex; flex-direction: column; height: 100%;", # 主面板填充剩余空间
        div(
          style = "flex-shrink: 0;", # 防止标题区域被压缩
          div(
            tags$span(icon("shopping-cart"), style = "margin-right: 5px;"),  # 使用 span 包裹图标
            "采购箱", 
            style = "font-size: 18px; font-weight: bold; color: #333; background-color: #c3d8fa; padding: 10px; text-align: center; border-radius: 4px;"
          )
        ),
        
        div(
          style = "flex-shrink: 0; padding-bottom: 20px;", # 确保表格区域高度固定
          column(12, DTOutput("added_items_table"))
        ),
        
        div(
          style = "flex-shrink: 0; padding: 20px 13px;",  # 固定按钮区域的高度
          fluidRow(
            column(2, style = "text-align: left;", uiOutput("add_update_button_ui")),            
            column(2, div(style = "text-align: right;",actionButton("confirm_btn", "确认登记", icon = icon("check"), class = "btn-primary", style = "width: 100%;"))),
            column(2, actionButton("delete_btn", "删除选中", icon = icon("trash"), class = "btn-danger", style = "width: 100%;")),
            column(6, div(textOutput("total_cost"),style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;"))
          )
        ),
        
        tags$hr(style = "margin: 20px 0; border: 1px solid #ddd;"),  # 添加分隔线
        
        div(
          style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 使表格内容填充剩余空间并支持滚动
          div(
            id = "item_table_container_purchase",
            uniqueItemsTableUI("unique_items_table_purchase")
          )
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
        itemFilterUI(id = "inbound_filter", border_color = "#28A745", text_color = "#28A745", status_choices = c("所有状态" = "", "采购", "国内入库")),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow(
          column(
            12,
            div(
              class = "card shadow-sm", # 添加卡片样式
              style = "border: 1px solid #007BFF; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
              
              # 卡片标题
              div(
                style = "margin-bottom: 10px; padding-bottom: 8px;",
                tags$h4("入库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 5px;"),
              ),
              
              # SKU 输入框
              div(
                style = "margin-bottom: 15px;",
                textInput(
                  "inbound_sku", 
                  label = NULL, 
                  placeholder = "请扫描或输入SKU",
                  width = "100%"
                ),
                checkboxInput(
                  "auto_inbound",  # 勾选框的 inputId
                  label = "自动入库（瑕疵信息不会采用）", 
                  value = FALSE  # 默认不勾选
                ),
              ),
              
              div(
                style = "width: 100%;",
                numericInput(
                  inputId = "inbound_quantity",
                  label = "入库数量",
                  value = 1,        # 默认值
                  min = 1,          # 最小值
                  max = 1,
                  step = 1          # 步长
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
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        fluidRow( 
          # 条形码生成下载按钮
          column(12,              
                 tags$div(
                   class = "card",
                   style = "padding: 15px; margin-bottom: 20px; border: 1px solid #007BFF; border-radius: 5px; box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);",
                   
                   # 卡片标题
                   div(
                     style = "margin-bottom: 10px; padding-bottom: 8px;",
                     tags$h4("条形码下载", style = "color: #007BFF; font-weight: bold; margin-bottom: 5px;"),
                   ),
                   
                   tags$div(
                     style = "display: flex; justify-content: space-between; align-items: center;",
                     actionButton("export_select_btn", "生成条形码", icon = icon("barcode"), class = "btn-info"),
                     downloadButton("download_select_pdf", "下载条形码", class = "btn-primary")
                   )
                 )
          )
        )
      ),
      
      div(
        class = "main-panel",
        
        div(
          style = "height: 300px; margin-bottom: 10px;",
          column(12, uiOutput("inbound_item_info"), style = "margin-bottom: 10px;") # 动态渲染物品信息
        ), 
        
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_inbound",
              uniqueItemsTableUI("unique_items_table_inbound")
            )
          )
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
        itemFilterUI(id = "outbound_filter", border_color = "#28A745", text_color = "#28A745", status_choices = c("所有状态" = "", "国内入库", "国内出库")),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("出库操作", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("outbound_sku", NULL, placeholder = "请扫描条形码操作，并核对物品", width = "100%"),
          checkboxInput(
            "auto_outbound",  # 勾选框的 inputId
            label = "自动出库", 
            value = FALSE  # 默认不勾选
          ),
          
          tags$div(
            class = "card",
            style = "padding: 15px; border: 2px solid #007BFF; border-radius: 8px; background-color: #f9f9f9; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
            tags$h4("选择国际运输方式:", style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;"),
            radioButtons(
              inputId = "outbound_shipping_method",
              label = NULL, # 将标签移到卡片标题
              choices = list("空运" = "空运", "海运" = "海运"),
              selected = "空运",  # 默认选择空运
              inline = TRUE       # 设置为横向排布
            )
          ),
          
          actionButton(
            "confirm_outbound_btn", 
            "确认出库", 
            icon = icon("check"), 
            class = "btn-primary", 
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          ),
          actionButton(
            "revert_outbound_btn",
            "撤回出库",
            icon = icon("undo"),
            class = "btn-warning",
            style = "font-size: 16px; width: 100%; height: 42px; margin-top: 10px;"
          )
        )
      ),
      
      div(
        class = "main-panel",
        div(
          style = "height: 300px; margin-bottom: 10px;",
          column(12, uiOutput("outbound_item_info"), style = "margin-bottom: 10px;") # 动态渲染物品信息
        ), 
        
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_outbound",
              uniqueItemsTableUI("unique_items_table_outbound")
            )
          )
        )
      )
    )
  ), # end of 出库 tab
  
  tabPanel(
    "售出", icon = icon("dollar-sign"),
    div(
      class = "layout-container",
      
      # 左侧：动态变化的筛选区和订单登记
      div(
        class = "sticky-sidebar",

        # 动态显示筛选区
        uiOutput("dynamic_sidebar"),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        # 订单登记区（共用）
        div(
          class = "card",
          style = "margin-bottom: 5px; padding: 15px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          tags$h4("订单登记与更新", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          fluidRow(
            column(
              7,
              textInput("order_id", "订单号", placeholder = "请输入订单号", width = "100%")
            ),
            column(
              5,
              selectInput(
                inputId = "platform",
                label = "电商平台",
                choices = c(
                  "请选择" = "",
                  "Etsy" = "Etsy",
                  "Shopify" = "Shopify",
                  "TikTok" = "TikTok",
                  "其他" = "其他"
                ),
                selected = "",
                width = "100%"
              )
            )
          ),
     
          textInput("customer_name", "顾客姓名", placeholder = "请输入或运单提取", width = "100%"),
          textInput("customer_netname", "顾客网名", placeholder = "请输入", width = "100%"),
          
          fluidRow(
            column(6, div(checkboxInput("is_transfer_order", "调货", value = FALSE))),
            column(6, div(checkboxInput("is_preorder", "预定", value = FALSE))),
          ),
          
          selectizeInput("preorder_supplier", "预定单供应商", choices = NULL, width = "100%", options = list(placeholder = '填选供应商...', maxOptions = 500)),
          
          # 运单号
          textInput("tracking_number", "运单号", placeholder = "输入运单号或运单提取", width = "100%"),
          
          # 运单PDF 文件上传组件
          fileInput("shiplabel_pdf_upload", "上传运单PDF", accept = ".pdf", width = "100%"),
          uiOutput("upload_status_message"),
          
          tags$div(style = "margin-top: 20px;"),  # 增加20px垂直间距
          
          # 订单图片上传
          imageModuleUI("image_sold", label = "订单图片上传", label_color = "#007BFF"),
          
          # 订单备注
          textAreaInput("order_notes", "订单备注", placeholder = "请输入备注内容", width = "100%"),
          
          # 按钮区
          div(
            style = "margin-top: 10px; display: flex; flex-direction: column; gap: 5px;",  # 增加垂直间距
            
            div(
              style = "display: flex; justify-content: space-between;",
              uiOutput("register_order_button_ui"),
              actionButton(
                "clear_order_btn",
                "清空订单",
                icon = icon("eraser"),
                class = "btn-warning",
                style = "font-size: 16px; width: 48%; height: 42px;"
              )
            ),
            
            div(
              style = "margin-top: 5px; display: flex; justify-content: center;",  # 设置行间距
              actionButton(
                "merge_order_btn",
                "合并订单",
                icon = icon("object-group"),
                class = "btn-primary",
                style = "font-size: 16px; width: 100%; height: 42px;"
              )
            )
          )
        )
      ),
      
      # 主面板：售出和订单管理的分页
      div(
        class = "main-panel",
        tabsetPanel(
          id = "sold_tabs",
          tabPanel(
            title = "物品售出",
            fluidRow(
              # 货架部分
              column(6,
                     div(
                       class = "card",
                       style = "padding: 10px; margin-bottom: 5px; border: 1px solid #007BFF; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                       
                       div(
                         style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 0px; height: 30px; padding: 0px",
                         
                         # 货架标题和动态显示数量
                         tags$h4(
                           HTML(paste0(
                             as.character(icon("warehouse")), 
                             "  货架  ",
                             span(style = "display: inline-flex; color: #007BFF; font-size: 18px;", textOutput("shelf_count")) # 动态显示数量
                           )),
                           style = "color: #007BFF; font-weight: bold;"
                         ),
                      
                         # 使用 Unicode 显示箭头
                         div(
                           style = "display: flex;",  # 使用 Flex 布局让内容在同一行显示
                           tags$label("排序:", style = "margin-right: 10px; font-weight: bold; font-size: 14px;"),  # 添加排序标签
                           radioButtons(
                             inputId = "arrow_direction",
                             label = NULL,  # 去掉默认的 radioButtons 标签
                             choices = list("↑" = "up", "↓" = "down"),  # Unicode 上箭头和下箭头
                             selected = "up",  # 默认选中上箭头
                             inline = TRUE  # 横向排列
                           ),
                           tags$style(HTML("
                            #arrow_direction.form-group {
                              margin-bottom: 0 !important; /* 移除默认的 margin-bottom */
                            }
                            #arrow_direction .radio-inline {
                              margin-right: 10px; /* 调整每个选项的间距 */
                            }
                          "))
                         ),
                         
                         # SKU 输入栏
                         textInput(
                           inputId = "sku_to_shelf",
                           label = NULL,  # 不显示标签
                           placeholder = "扫码上架",  # 提示文字
                           width = "200px"  # 控制输入框宽度
                         ),
                         tags$style(HTML("
                          #sku_to_shelf {
                            height: 35px !important;  /* 调整高度 */
                            font-size: 15px;          /* 调整字体大小 */
                            padding: 5px;             /* 调整内边距 */
                          }
                        "))
                       ),
                       
                       DTOutput("shelf_table")  # 显示货架上的物品
                     )
              ),
              
              # 箱子部分
              column(6,
                     div(
                       class = "card",
                       style = "padding: 10px; margin-bottom: 5px; border: 1px solid #28A745; border-radius: 10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                       
                       div(
                         style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 0px; height: 30px",
                         
                         # 发货箱标题和动态显示数量
                         tags$h4(
                           HTML(paste0(
                             as.character(icon("box")), 
                             "  发货箱  ",
                             span(style = "display: inline-flex; color: #28A745; font-size: 18px;", textOutput("box_count")) # 动态显示数量
                           )),
                           style = "color: #28A745; font-weight: bold; margin: 0;"
                         ),
                         
                         # SKU 输入栏
                         textInput(
                           inputId = "sku_to_box",
                           label = NULL,  # 不显示标签
                           placeholder = "扫码入箱",  # 提示文字
                           width = "200px"  # 控制输入框宽度
                         ),
                         tags$style(HTML("
                          #sku_to_box {
                            height: 35px !important;  /* 调整高度 */
                            font-size: 15px;          /* 调整字体大小 */
                            padding: 5px;             /* 调整内边距 */
                          }
                        "))
                       ),
                       
                       DTOutput("box_table"),  # 显示已放入箱子的物品
                       
                       fluidRow(
                         column(
                           width = 6, # 左侧按钮宽度
                           actionButton(
                             "confirm_order_btn",
                             "确认售出",
                             icon = icon("check"),
                             class = "btn-primary",
                             style = "font-size: 16px; width: 100%; height: 50px; margin-top: 10px;"
                           )
                         ),
                         column(
                           width = 6, # 右侧选择框宽度
                           tags$div(
                             style = "
                              display: flex;
                              align-items: center;
                              justify-content: flex-start;
                              border: 1px solid #007BFF;
                              border-radius: 8px;
                              height: 50px;
                              padding: 0 10px;
                              margin-top: 10px;
                            ",
                             tags$span(
                               "国际运输:",
                               style = "font-size: 16px; font-weight: bold; margin-right: 15px; line-height: 1;"
                             ),
                             tags$div(
                               style = "
                                  display: flex;
                                  align-items: center;
                                  height: 100%;
                                  margin-bottom: 0; /* 移除底部间距 */
                                ",
                               tags$style(HTML("
                                  #sold_shipping_method .radio {
                                    margin-bottom: 0 !important; /* 移除默认的 margin */
                                  }
                                  #sold_shipping_method {
                                    margin-bottom: 0 !important; /* 避免容器本身多余间距 */
                                  }
                                ")),
                               radioButtons(
                                 inputId = "sold_shipping_method",
                                 label = NULL, # 去掉默认 label
                                 choices = list("空运" = "空运", "海运" = "海运"),
                                 selected = "空运",  # 默认选择空运
                                 inline = TRUE       # 设置为横向排布
                               )
                             )
                           )
                         )
                       )
                     )
              )
            ),
            
            tags$hr(style = "margin: 5px 0; border: 1px solid #ddd;"),  # 添加分隔线
            
            div(
              style = "display: flex; flex-direction: column;",
              div(
                style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
                div(
                  id = "item_table_container_sold",
                  uniqueItemsTableUI("unique_items_table_sold")
                )
              )
            )
          ),
          tabPanel(
            title = "订单管理",
            div(
              class = "card",
              style = "height: 460px; padding: 5px; border: 1px solid #ccc; border-radius: 8px;", # 自动调整高度
              orderTableUI("orders_table_module")
            ),
            div(
              class = "card",
              style = "padding: 5px; border: 1px solid #ccc; border-radius: 8px;", # 自动调整高度
              uiOutput("associated_items_title"),  # 动态标题
              uiOutput("order_items_cards")  # 动态显示订单内物品卡片
            )
          )
        )
      )
    )
  ), # End of 售出
  
  
  tabPanel(
    "物品管理", icon = icon("list-check"),
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        itemFilterUI(id = "manage_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = TRUE),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        div(
          class = "card shadow-sm", # 添加卡片样式
          style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 20px; background-color: #f9f9f9;",
          # 卡片标题
          div(
            style = "margin-bottom: 10px; padding-bottom: 8px;",
            tags$h4("更新选中商品信息", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;")
          ),
          
          # 图片模块
          imageModuleUI("image_manage", label = "更新商品图片"),
          
          actionButton("update_image_btn", "更新图片", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;"),
          
          tags$hr(style = "margin: 10px 0; border: none;"),
          
          fluidRow(
            column(6, numericInput("update_product_cost", "修改单价", value = NULL, min = 0, width = "100%")),
            column(6, numericInput("update_shipping_cost", "修改国内运费", value = NULL, min = 0, width = "100%"))
          ),
          
          # 按钮
          fluidRow(
            column(7, actionButton("update_info_btn", "更新单价/运费", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;")),
            column(5, actionButton("clear_info_btn", "清空", icon = icon("eraser"), style = "background-color: #8B0000; color: white; width: 100%;"))
          )
        ),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
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
      
      div(
        class = "main-panel",
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_manage",
              uniqueItemsTableUI("unique_items_table_manage")
            )
          )
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

        itemFilterUI(id = "defect_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = TRUE),
        
        tags$hr(), # 分隔线
        
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
          tags$label("仅显示无瑕品", class = "control-label"),  
          switchInput(
            inputId = "show_perfects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        tags$hr(), # 分隔线
        
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
          tags$label("仅显示瑕疵品", class = "control-label"),  
          switchInput(
            inputId = "show_defects_only",  # 开关 ID
            label = NULL,                   # 不显示标签在开关上
            value = FALSE                   # 默认值：关闭
          )
        ),
        
        tags$hr(), # 分隔线
        
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
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_defect",
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
        
        itemFilterUI(id = "logistic_filter", 
                     use_purchase_date = FALSE,
                     use_sold_date = TRUE, use_exit_date = TRUE,
                     border_color = "#28A745", text_color = "#28A745"),
        
        tags$hr(style = "margin: 5px 0; border: none;"),
        
        div(
          class = "card shadow-sm",
          style = "padding: 10px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          
          # Card 标题
          tags$h4("登记国际运单", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          
          # 输入内容
          fluidRow(
            column(12, textInput("intl_tracking_number", "国际运单号:", placeholder = "请输入空运或海运运单号", width = "100%")),
            column(12, selectInput("intl_shipping_method", "国际运输方式:", choices = c("空运" = "空运", "海运" = "海运"), selected = "空运", width = "100%")),
            column(12, numericInput("intl_total_shipping_cost", "国际物流总运费 (元):", value = 0, min = 0, width = "100%"))
          ),
          
          fluidRow(
            column(4, actionButton("register_shipment_btn", "登记", icon = icon("save"), class = "btn-info", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
            column(4, actionButton("batch_value_btn", "货值", icon = icon("dollar-sign"), class = "btn-success", style = "margin-top: 20px; width: 100%; font-size: 16px;")),
            column(4, actionButton("delete_shipment_btn", "删除", icon = icon("trash"), class = "btn-danger", style = "margin-top: 20px; width: 100%; font-size: 16px;"))
          )
        ),
        
        fluidRow(
          column(6, actionButton("link_tracking_btn", "挂靠运单", icon = icon("link"), class = "btn-primary", style = "margin-top: 20px; width: 100%;", disabled = TRUE)),
          column(6, actionButton("delete_tracking_btn", "解除挂靠", icon = icon("link-slash"), class = "btn-danger", style = "margin-top: 20px; width: 100%;"))
        )
      ),
      div(
        class = "main-panel",
        div(
          style = "display: flex; flex-direction: column;",
          div(
            style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
            div(
              id = "item_table_container_logistics",
              uniqueItemsTableUI("unique_items_table_logistics")
            )
          )
        )
      )
    )
  ), # end of 国际物流管理 tab
  
  tabPanel(
    "账务管理", icon = icon("wallet"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        tabsetPanel(
          id = "sidebar_tabs",  # 用于服务器监听当前选中的分页
          type = "tabs",        # 使用标签式分页
          selected = "账务登记", # 默认选中的分页
          
          # 账务登记分页
          tabPanel(
            title = "账务登记", icon = icon("file-invoice-dollar"),
            tags$h4("账务登记", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
            
            # 单一金额输入框
            numericInput("amount", "金额:", value = 0, min = 0, width = "100%"),
            
            # 互斥勾选框
            radioButtons(
              inputId = "transaction_type",
              label = "交易类型:",
              choices = c("转出" = "out", "转入" = "in"),
              selected = NULL,
              inline = TRUE
            ),
            
            # 指定转款选择器
            fluidRow(
              column(5, dateInput("custom_date", "转款日期:", value = Sys.Date(), width = "100%")),
              column(7, timeInput("custom_time", "转款时间:", value = format(Sys.time(), "%H:%M:%S"), width = "100%"))
            ),
            
            # 订单图片上传
            imageModuleUI("image_transactions", label = "转账证据上传", label_color = "#007BFF"),
            
            textAreaInput("remarks", "备注:", placeholder = "请输入备注内容", width = "100%"),
            
            # 提交按钮
            actionButton("record_transaction", "登记", icon = icon("save"), 
                         class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            
            # 删除和重置按钮同一行
            fluidRow(
              column(
                width = 6,
                actionButton("delete_transaction", "删除选中记录", icon = icon("trash"), 
                             class = "btn-danger", style = "width: 100%;")
              ),
              column(
                width = 6,
                actionButton("reset_form", "重置", icon = icon("redo"), 
                             class = "btn-info", style = "width: 100%;")
              )
            )
          ),
          
          # 资金转移分页
          tabPanel(
            title = "资金转移", icon = icon("exchange-alt"),
            tags$h4("资金转移", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
            
            # 转移金额输入框
            numericInput("transfer_amount", "转移金额:", value = NULL, min = 0, width = "100%"),
            
            # 转出账户选择
            selectInput(
              inputId = "from_account",
              label = "转出账户:",
              choices = c("工资卡", "美元卡", "买货卡", "一般户卡"),
              selected = "美元卡",
              width = "100%"
            ),
            
            # 转入账户选择
            selectInput(
              inputId = "to_account",
              label = "转入账户:",
              choices = c("工资卡", "美元卡", "买货卡", "一般户卡"),
              selected = NULL,
              width = "100%"
            ),
            
            # 订单图片上传
            imageModuleUI("image_transfer", label = "转账证据上传", label_color = "#007BFF"),
            
            # 备注输入框
            textAreaInput("transfer_remarks", "备注:", placeholder = "请输入备注内容", width = "100%"),
            
            # 转移登记按钮
            actionButton("record_transfer", "记录转移", icon = icon("exchange-alt"), 
                         class = "btn-success", style = "width: 100%;"),
            
            # 删除和重置按钮同一行
            fluidRow(
              column(
                width = 6,
                actionButton("delete_transfer", "删除选中记录", icon = icon("trash"), 
                             class = "btn-danger", style = "width: 100%;")
              ),
              column(
                width = 6,
                actionButton("reset_form_transfer", "重置", icon = icon("redo"), 
                             class = "btn-info", style = "width: 100%;")
              )
            )
          )
        )
      ),
      div(
        class = "main-panel",
        tabsetPanel(
          id = "transaction_tabs",  # 绑定到 input$tabs
          tabPanel("账户余额总览", 
          fluidRow(
           column(12, div(
             class = "card shadow-lg",
             style = "background: #1F1F1F; color: white; padding: 40px; text-align: center; border-radius: 16px; margin-bottom: 40px; border: 2px solid #FFC107;",
             tags$h4("总余额", style = "font-weight: bold; font-size: 30px; margin-bottom: 20px; letter-spacing: 1.5px;"),
             tags$h3(
               textOutput("total_balance"),
               style = "font-size: 40px; margin-top: 0; font-weight: bold; text-shadow: 2px 2px 4px rgba(255, 193, 7, 0.8); color: #FFC107;"
             )
           ))
          ),
          fluidRow(
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #FFC107, #FF9800); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("买货卡 (139)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("purchase_balance"), style = "font-size: 24px; margin-top: 0;")
            )),
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #6C757D, #495057); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("一般户卡 (541)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("general_balance"), style = "font-size: 24px; margin-top: 0;")
            )),
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #007BFF, #0056b3); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("工资卡 (567)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("salary_balance"), style = "font-size: 24px; margin-top: 0;")
            )),
            column(3, div(
              class = "card shadow-lg",
              style = "background: linear-gradient(135deg, #28A745, #1E7E34); color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;",
              tags$div(
                style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
              ),
              tags$h4("美元卡 (553)", style = "font-weight: bold; margin-bottom: 10px;"),
              tags$h3(textOutput("dollar_balance"), style = "font-size: 24px; margin-top: 0;")
            ))
          )),
          tabPanel(title = "买货卡(139)", value = "买货卡", DTOutput("purchase_card_table")),
          tabPanel(title = "一般户卡(541)", value = "一般户卡", DTOutput("general_card_table")),
          tabPanel(title = "工资卡(567)", value = "工资卡", DTOutput("salary_card_table")),
          tabPanel(title = "美元卡(553)", value = "美元卡", DTOutput("dollar_card_table"))
        )
      )
    )
  ), # End of 账务管理
  
  tabPanel(
    "查询", icon = icon("search"), 
    div(
      class = "layout-container",  # Flexbox 容器
      div(
        class = "sticky-sidebar",  # sticky 侧边栏
        
        itemFilterUI(id = "query_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = FALSE),
        
        tags$hr(),
        
        div(
          class = "card",
          style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
          tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
          textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
          actionButton("clear_query_sku_btn", "清空", icon = icon("eraser"), class = "btn btn-warning")
        )
      ),
      div(
        class = "main-panel",
        # 使用 tabsetPanel 来组织分页
        tabsetPanel(
          id = "query_tabs",
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
              )
            ),
            
            div(
              style = "display: flex; flex-direction: column;",
              div(
                style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",  # 表格自适应高度
                div(
                  id = "inventory_table_container_query",
                  DTOutput("filtered_inventory_table_query")
                )
              )
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
                    column(3,                   
                           dateRangeInput(
                             "time_range",
                             label = "选择采购时间范围",
                             start = Sys.Date() - 30, # 默认最近30天
                             end = Sys.Date()
                           )),
                    column(3,
                           radioButtons(
                             "precision",
                             label = "选择统计精度",
                             choices = c("天" = "天", "周" = "周", "月" = "月", "年" = "年"),
                             selected = "天",
                             inline = TRUE # 使选项横向排列
                           )),
                    column(5,
                           radioButtons(
                             "expense_type",
                             label = "选择显示内容",
                             choices = c("物品成本+国内运费" = "cost_domestic", "物品成本" = "cost", "国内运费" = "domestic_shipping", "国际运费" = "intl_shipping", "总开销" = "total"),
                             selected = "cost_domestic",
                             inline = TRUE # 使选项横向排列
                           )),
                    column(1,
                           actionButton(
                             "reset_time_range",
                             label = "",
                             icon = icon("redo"), # 添加一个重置图标
                             class = "btn-warning", # 设置按钮样式
                             style = "height: 50px; font-size: 14px;" # 设置样式
                           ))
                  ),
                  
                  # 图表行：柱状图 + 饼图
                  fluidRow(
                    column(9, plotlyOutput("expense_chart", height = "350px")), # 80% 宽度柱状图
                    column(3, plotlyOutput("pie_chart", height = "350px"))  # 20% 宽度饼图
                  ),
                  uiOutput("confirm_expense_check_ui"),
                  uniqueItemsTableUI("expense_details_table") # 物品详情表
                )
              )
            )
          ), # end of 开销汇总tab
          
          
          tabPanel(
            "库存总览",
            fluidRow(
              # 国内库存卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("国内库存", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("domestic_total_count"), style = "color: #007BFF; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("domestic_total_value"), style = "color: #007BFF;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("domestic_shipping_cost"), style = "color: #007BFF;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 国际物流卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("国际物流", style = "color: #28A745; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("logistics_total_count"), style = "color: #28A745; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("logistics_total_value"), style = "color: #28A745;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("logistics_shipping_cost"), style = "color: #28A745;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 美国库存卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #6F42C1; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("美国库存", style = "color: #6F42C1; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(textOutput("us_total_count"), style = "color: #6F42C1; font-weight: bold;"),
                    tags$p("物品总数")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("us_total_value"), style = "color: #6F42C1;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("us_shipping_cost"), style = "color: #6F42C1;"),
                    tags$p("运输成本")
                  )
                )
              ),
              # 商品售出卡片
              column(
                3,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #FF5733; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("商品售出", style = "color: #FF5733; font-weight: bold; text-align: center;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h3(
                      textOutput("sold_total_count_with_shipping"),
                      style = "color: #FF5733; font-weight: bold;"
                    ),
                    tags$p("物品总数（已投递）")
                  )
                  ,
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("sold_total_value"), style = "color: #FF5733;"),
                    tags$p("货物价值")
                  ),
                  tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$h4(textOutput("sold_shipping_cost"), style = "color: #FF5733;"),
                    tags$p("运输成本")
                  )
                )
              )
            ),
            
            tags$hr(style = "margin: 10px 0; border: 1px solid #ddd;"),
            
            fluidRow(
              column(
                12,
                div(
                  class = "card",
                  style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("库存状态流转桑基图", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                  sankeyNetworkOutput("status_sankey", height = "345px")
                )
              )
            )
            
          ) # end of 库存汇总tab
          
          
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
          uiOutput("download_maker_ui"),  # 动态生成供应商筛选器,
          
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
            start = Sys.Date() - 365, # 默认最近365天
            end = Sys.Date(),        # 默认结束日期为今天
            format = "yyyy-mm-dd",   # 日期格式
            separator = " 至 ",
            width = "100%"
          ),
          
          actionButton("download_reset_filters", "重置筛选", class = "btn-secondary")
        ),
        
        tags$hr(),
        
        downloadButton(
          outputId = "download_summary_xlsx",
          label = "下载物品汇总表（按采购日期）",
          class = "btn-primary",
          style = "width: 100%; margin-top: 10px;"
        ),
        
        downloadButton(
          outputId = "download_details_xlsx",
          label = "下载物品明细表",
          class = "btn-primary",
          style = "width: 100%; margin-top: 10px;"
        )
        
        
      ),
      div(
        class = "main-panel",
        uniqueItemsTableUI("unique_items_table_download")
      )
    )
  ), # End of 数据下载 tab
  
  tabPanel(
    "管理员", icon = icon("user-shield"),
    div(
      class = "layout-container",
      div(
        class = "sticky-sidebar",
        passwordInput("admin_password", "请输入管理员密码：", width = "100%"),
        actionButton("admin_login_btn", "登录", icon = icon("unlock"), class = "btn-primary", style = "width: 100%; margin-top: 10px;"),
        tags$hr(),
        uiOutput("admin_controls")
      ),
      div(
        class = "main-panel",
        uniqueItemsTableUI("admin_items_table")  # 使用你的模组渲染物品明细表
      )
    )
  ) # End of 管理员 tab
)
