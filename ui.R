# Define UI
ui <- navbarPage(
  title = "ERP系统（国内端）",
  id = "inventory_cn",  # 设置 ID，用于监听当前选中的主页面
  theme = shinytheme("flatly"), # 可选主题
  position = "fixed-top",
  
  header = tagList(
    useShinyjs(),  # 启用 shinyjs 功能
    
    # 物品表刷新（联动刷新库存表与订单表）
    actionButton("refresh_global_items_btn", "", icon = icon("sync"), class = "btn-success", style = "position: fixed; top: 8px; right: 20px; z-index: 9999;"),
    
    # 加载动画界面
    tags$div(id = "loading-screen", style = "position: fixed; width: 100%; height: 100%; background: white; z-index: 9999; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center;",
             tags$img(src = "https://www.goldenbeanllc.com/icons/spinning_yarn.gif", style = "width: 80px; height: 80px;"),
             tags$p("系统加载中，请稍后...", style = "font-size: 18px; font-weight: bold; color: #333; margin-top: 10px;")
    ),
    
    # 库存状态浮动框（协作页）
    tags$div(id = "inventory-status-popup", style = "display: none; position: absolute; z-index: 9999; background: white; border: 1px solid #ccc; padding: 5px; box-shadow: 2px 2px 8px rgba(0,0,0,0.2); border-radius: 5px; min-width: 220px; min-height: 220px;",
             plotlyOutput("colab_inventory_status_chart", width = "220px", height = "220px")
    ),
    
    tags$head(tags$link(rel = "icon", type = "image/x-icon", href = "https://www.goldenbeanllc.com/icons/favicon-96x96.png")),  # 设置页面图标
    
    # CSS 样式定义
    tags$style(HTML("
            #loading-screen{transition:opacity 1s ease-out;}  /* 加载画面淡出动画 */
            .navbar-nav{display:flex !important;flex-wrap:nowrap !important;overflow-x:auto !important;white-space:nowrap !important;max-width:100% !important;}  /* 导航栏水平滚动 */
            .navbar-nav::-webkit-scrollbar{height:6px;} .navbar-nav::-webkit-scrollbar-thumb{background:#007BFF;border-radius:10px;}  /* 滚动条样式 */
            @media (max-width:1500px){.navbar-nav{overflow-x:scroll !important;}.navbar-brand{display:none !important;}}  /* 小屏幕隐藏标题 */
            @media (max-width:980px){.navbar-nav > li > a{font-size:12px !important;padding:6px 8px !important;}}  /* 小屏幕调整字体 */
            .navbar{display:block !important;overflow:hidden !important;width:100% !important;}  /* 导航栏宽度限制 */
            body{padding-top:70px !important;}  /* 顶部留白 */
            .layout-container{display:flex;flex-direction:row;height:100%;width:100%;overflow:visible;}  /* Flexbox 布局容器 */
            .sticky-sidebar{position:sticky;top:70px;z-index:900;flex:0 0 auto;width:380px;min-width:280px;max-width:580px;height:calc(100vh - 70px);overflow-y:auto;border-right:1px solid #e0e0e0;border-radius:8px;padding:20px;background-color:#f9f9f9;transition:width 0.2s ease;}  /* 侧边栏样式 */
            .main-panel{flex-grow:1;overflow:auto;padding:20px;padding-top:0;background-color:#ffffff;transition:width 0.2s ease;}  /* 主面板样式 */
            .resizable-divider{background-color:#aaa;width:5px;cursor:ew-resize;flex-shrink:0;}  /* 可拖拽分隔条 */
            table.dataTable thead th{white-space:nowrap;}  /* 数据表头不换行 */
            div.dataTables_wrapper div.dataTables_filter{text-align:left !important;float:left !important;} div.dataTables_wrapper div.dataTables_filter label{display:inline-flex;align-items:center;gap:5px;}  /* DT 搜索框左对齐 */
            .arrow-icon{margin-right:10px;}  /* 采购流程箭头 */
            .status-badge{display:inline-block;padding:2px 8px;border-radius:6px;font-size:12px;font-weight:bold;color:white;text-align:center;margin-left:10px;min-width:24px;}  /* 状态徽章样式 */
            .status-existing{background-color:#28A745;} .status-new{background-color:#FFA500;}  /* 状态颜色 */
            .note-card{display:flex !important;opacity:1 !important;} .pagination-controls{display:flex;align-items:center;gap:10px;margin-top:20px;justify-content:center;}  /* 其他辅助样式 */
          ")),
    
    # JavaScript 功能实现
    tags$script(HTML("
            $(document).ready(function(){  // 页面加载后执行
              $('#loading-screen').css('transition','opacity 1s ease-out');  // 设置加载画面淡出效果
              $('#refresh_global_items_btn').on('click',function(){Shiny.setInputValue('refresh_item_table',new Date().getTime(),{priority:'event'});});  // 刷新按钮触发事件
              $('#transaction_amount').attr('placeholder','成交额（$）');  // 设置成交额输入框占位符
              $('#filtered_inventory_table_query').on('contextmenu','tr',function(event){  // 库存表右键菜单
                event.preventDefault();
                var rowIdx=$(this).index();
                Shiny.setInputValue('selected_inventory_row',rowIdx+1,{priority:'event'});
                $('#context-menu').css({display:'block',left:event.pageX+'px',top:event.pageY+'px'});
              });
              $(document).on('click',function(event){if(!$(event.target).closest('#context-menu').length)$('#context-menu').hide();});  // 点击空白处隐藏右键菜单
            });
            
            // 库存状态浮窗显示逻辑
            let inventoryStatusTimeout;
            function showInventoryStatus(event,sku){
              clearTimeout(inventoryStatusTimeout);
              Shiny.setInputValue('hover_sku',sku,{priority:'event'});
              inventoryStatusTimeout=setTimeout(function(){
                var popup=document.getElementById('inventory-status-popup');
                if(sku==='New-Request')popup.style.display='none';
                else{popup.style.display='block';popup.style.left=(event.pageX+20)+'px';popup.style.top=(event.pageY+20)+'px';}
              },1000);
            }
            function hideInventoryStatus(){
              clearTimeout(inventoryStatusTimeout);
              document.getElementById('inventory-status-popup').style.display='none';
            }
            
            // 粘贴图片功能
            $(document).on('paste','[id$=\"paste_area\"]',function(event){
              const items=(event.originalEvent.clipboardData||event.clipboardData).items;
              for(let i=0;i<items.length;i++){
                if(items[i].type.indexOf('image')!==-1){
                  const file=items[i].getAsFile();
                  const reader=new FileReader();
                  reader.onload=function(evt){
                    const inputId=event.currentTarget.id+'_pasted_image';
                    Shiny.setInputValue(inputId,evt.target.result,{priority:'event'});
                  };
                  reader.readAsDataURL(file);
                  break;
                }
              }
            });
            
            // 可拖拽分隔条功能
            document.addEventListener('DOMContentLoaded',function(){
              function enableResizing(divider){
                const sidebar=divider.previousElementSibling;
                let isResizing=false;
                divider.addEventListener('mousedown',function(e){isResizing=true;document.body.style.cursor='ew-resize';document.body.style.userSelect='none';});
                document.addEventListener('mousemove',function(e){
                  if(!isResizing)return;
                  const newSidebarWidth=Math.max(200,Math.min(600,e.clientX));
                  sidebar.style.flex=`0 0 ${newSidebarWidth}px`;
                  $('.dataTable').DataTable().columns.adjust();
                });
                document.addEventListener('mouseup',function(){
                  if(isResizing){isResizing=false;document.body.style.cursor='';document.body.style.userSelect='';$('.dataTable').DataTable().columns.adjust();}
                });
              }
              function bindResizableDividers(){
                document.querySelectorAll('.resizable-divider').forEach(function(divider){
                  if(!divider.dataset.bound){enableResizing(divider);divider.dataset.bound=true;}
                });
              }
              bindResizableDividers();
              $(document).on('shown.bs.tab',function(){bindResizableDividers();$('.dataTable').DataTable().columns.adjust();});
            });
            
            // 音效播放函数
            function playSound(type){var audio=new Audio('https://www.goldenbeanllc.com/sounds/'+type+'-8bit.mp3');audio.play();}
            function playSuccessSound(){playSound('success');}  // 成功音效
            function playErrorSound(){playSound('error');}  // 错误音效
          "))
  ),
  
  tabPanel("协作", icon = icon("handshake"),
           div(class = "layout-container",
               # 左侧侧边栏
               div(class = "sticky-sidebar",
                   div(
                     tags$h4("库存品请求", style = "font-weight: bold; color: #007BFF;"),
                     fluidRow(column(6, textInput("search_sku", "按SKU搜索", placeholder = "输入SKU", width = "100%")), column(6, textInput("search_name", "按物品名搜索", placeholder = "输入物品名", width = "100%"))),
                     div(style = "margin-bottom: 10px;", div(style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;", tags$h5("物品预览", style = "font-weight: bold; color: #007BFF;"), uiOutput("item_preview"))),
                     numericInput("request_quantity", "请求数量", value = 0, min = 1, width = "100%"), textAreaInput("request_remark", "留言", placeholder = "（选填）", width = "100%", height = "60px"), actionButton("add_request", "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;"),
                     tags$hr(),
                     tags$h4("新商品请求", style = "font-weight: bold; color: #007BFF;"), imageModuleUI("image_requests", label = "请求物品图片上传"), textInput("custom_description", "物品名", placeholder = "输入物品名", width = "100%"), numericInput("custom_quantity", "请求数量", value = 0, min = 1, width = "100%"), textAreaInput("custom_remark", "留言", placeholder = "（选填）", width = "100%", height = "60px"), actionButton("submit_custom_request", "创建请求", icon = icon("plus"), class = "btn-success", style = "width: 100%; margin-top: 10px;")
                   )
               ),
               # 可调整的分割线
               div(class = "resizable-divider"),
               # 右侧主要面板
               div(class = "main-panel",
                   div(style = "display: flex; align-items: flex-end; gap: 5px; margin: 0; padding: 0; max-width: 300px;", div(style = "max-width: 250px;", uiOutput("supplier_filter")), actionButton(inputId = "reset_supplier", label = NULL, icon = icon("refresh"), class = "btn-info btn-sm", style = "margin-bottom: 14px; height: 34px;")),
                   # 采购流程 tabset
                   tabsetPanel(id = "collaboration_tabs", type = "pills",
                               tabPanel(value = "purchase", title = "采购请求", uiOutput("purchase_request_board")),
                               tabPanel(value = "arranged", title = div(tags$span(class = "arrow-icon", icon("arrow-right")), "已安排"), uiOutput("provider_arranged_board")),
                               tabPanel(value = "completed", title = div(tags$span(class = "arrow-icon", icon("arrow-right")), "已完成"), uiOutput("done_paid_board")),
                               tabPanel(value = "outbound", title = div(tags$span(class = "arrow-icon", icon("arrow-right")), "待出库"), uiOutput("outbound_request_board")),
                               tabPanel(value = "new_product", title = "新品请求", uiOutput("new_product_board"))
                   )
               )
           )
  ), # End of 协作 tab
  
  tabPanel(
    "采购", icon = icon("shopping-cart"),
    div(class = "layout-container",  # Flexbox 容器
        div(class = "sticky-sidebar",  # sticky 侧边栏
            itemFilterUI(id = "purchase_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = FALSE, use_status = FALSE),
            actionButton("reset_btn", "重置采购登记", icon = icon("snowplow"), class = "btn-info", 
                         style = "font-size: 14px; width: 100%; height: 45px; padding: 0; margin: 15px 0;"),
            fluidRow(
              column(10, selectizeInput("new_maker", NULL, choices = NULL, width = "100%", 
                                        options = list(placeholder = '供应商名称(拼音)', maxOptions = 500))),
              column(2, div(style = "display: flex; justify-content: flex-start; align-items: center; height: 100%;", 
                            actionButton("add_supplier_btn", label = NULL, icon = icon("plus"), 
                                         style = "font-size: 14px; width: 100%; height: 34px; padding: 0; margin: 0;")))
            ),
            typeModuleUI("type_module"),
            fluidRow(
              column(12, autocompleteInputUI("purchase", NULL, placeholder = "请输入商品名...")),
              column(12, h5("预订单物品备忘（点击自动填写）", style = "color: #17a2b8;"),
                     div(style = "display: flex; align-items: center; gap: 0;",
                         textInput("preorder_item_search_filter", NULL, value = "", placeholder = "搜索物品名或供应商...", width = "100%"),
                         actionButton("clear_preorder_search_box", label = "", icon = icon("xmark", style = "color: #D32F2F;"), 
                                      style = "padding: 0 5px; border: none; margin-bottom: 14px; font-size: 18px; background-color: #F5F5F5; height: 45px; min-width: 34px;")),
                     div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f5f5f5; max-height: 200px; overflow-y: auto; margin-bottom: 15px;",
                         uiOutput("preorder_items_memo")),
                     dateInput(inputId = "purchase_date", label = "采购日期", value = Sys.Date(), width = "100%"))
            ),
            fluidRow(
              column(4, numericInput("new_quantity", "数量", value = 0, min = 0, step = 1)),
              column(4, numericInput("new_product_cost", "单价", value = 0, min = 0)),
              column(4, numericInput("new_shipping_cost", "运费", value = 0, min = 0))
            ),
            fluidRow(column(12, textInput("new_sku", NULL, placeholder = "SKU(自动生成)", value = "", width = "100%"))),
            imageModuleUI("image_purchase")
        ),
        div(class = "resizable-divider"),
        div(class = "main-panel", style = "display: flex; flex-direction: column; height: 100%;",  # 主面板填充剩余空间
            div(style = "flex-shrink: 0;",  # 防止标题区域被压缩
                div(tags$span(icon("shopping-cart"), style = "margin-right: 5px;"), "采购箱", 
                    style = "font-size: 18px; font-weight: bold; color: #333; background-color: #c3d8fa; padding: 10px; text-align: center; border-radius: 4px;")),
            div(style = "flex-shrink: 0; padding-bottom: 20px;", column(12, DTOutput("added_items_table"))),  # 表格区域
            div(style = "flex-shrink: 0; padding: 20px 13px;",  # 按钮区域
                fluidRow(
                  column(2, style = "text-align: left;", uiOutput("add_update_button_ui")),
                  column(2, div(style = "text-align: right;", actionButton("confirm_btn", "确认登记", icon = icon("check"), class = "btn-primary", style = "width: 100%;"))),
                  column(2, actionButton("delete_btn", "删除选中", icon = icon("trash"), class = "btn-danger", style = "width: 100%;")),
                  column(6, div(textOutput("total_cost"), style = "font-size: 20px; font-weight: bold; color: blue; text-align: center;")))
            ),
            tags$hr(style = "margin: 20px 0; border: 1px solid #ddd;"),  # 分隔线
            div(id = "item_table_container_purchase", uniqueItemsTableUI("unique_items_table_purchase"))
        )
    )
  ), # end of 采购登记 tab
  
  tabPanel("入库", icon=icon("arrow-circle-down"),
           div(class="layout-container",
               div(class="sticky-sidebar",
                   itemFilterUI(id="inbound_filter", border_color="#28A745", text_color="#28A745", status_choices=c("库存状态"="", "采购", "国内入库")),
                   tags$hr(style="margin:5px 0;border:none;"),
                   fluidRow(
                     column(12,
                            div(class="card shadow-sm", style="border:1px solid #007BFF;border-radius:8px;padding:20px;background-color:#f9f9f9;",
                                div(style="margin-bottom:10px;padding-bottom:8px;", tags$h4("入库操作", style="color:#007BFF;font-weight:bold;margin-bottom:5px;")),
                                div(style="margin-bottom:15px;",
                                    div(style="display:flex;align-items:center;gap:0px;",
                                        textInput("inbound_sku", label=NULL, placeholder="请输入商品SKU", width="100%"),
                                        actionButton("clear_inbound_sku", label="", icon=icon("xmark", style="color:#D32F2F;"), style="padding:0 5px;border:none;margin-bottom:14px;font-size:18px;background-color:#F5F5F5;height:45px;min-width:34px;")
                                    ),
                                    checkboxInput("auto_inbound", label="自动入库（瑕疵信息不会采用）", value=FALSE),
                                    conditionalPanel(condition="input.auto_inbound == true", checkboxInput("speak_inbound_item_name", "念出商品名", value=FALSE))
                                ),
                                div(style="width:100%;", numericInput(inputId="inbound_quantity", label="入库数量", value=1, min=1, max=1, step=1)),
                                div(style="margin-bottom:20px;display:flex;align-items:center;",
                                    tags$input(type="checkbox", id="defective_item", style="width:20px;height:20px;margin-right:10px;"),
                                    tags$label("瑕疵品", `for`="defective_item", style="font-size:18px;font-weight:bold;color:#444;")
                                ),
                                div(id="defective_notes_container", style="display:none;margin-top:10px;",
                                    textAreaInput(inputId="defective_notes", label="瑕疵品备注：", placeholder="请输入备注内容...", width="100%")
                                ),
                                actionButton("confirm_inbound_btn", "确认入库", icon=icon("check"), class="btn-primary", style="font-size:16px;width:100%;height:42px;")
                            )
                     )
                   ),
                   tags$hr(style="margin:5px 0;border:none;"),
                   fluidRow(
                     column(12,
                            tags$div(class="card", style="padding:15px;margin-bottom:20px;border:1px solid #007BFF;border-radius:5px;box-shadow:0 2px 5px rgba(0,0,0,0.1);",
                                     div(style="margin-bottom:10px;padding-bottom:8px;", tags$h4("条形码下载", style="color:#007BFF;font-weight:bold;margin-bottom:5px;")),
                                     downloadButton("download_barcode_pdf", "生成并下载条形码 PDF", class="btn-info", style="width:100%;")
                            )
                     )
                   )
               ),
               div(class="resizable-divider"),
               div(class="main-panel",
                   div(style="height:300px;margin-bottom:10px;", column(12, uiOutput("inbound_item_info"), style="margin-bottom:10px;")),
                   div(id="item_table_container_inbound", uniqueItemsTableUI("unique_items_table_inbound"))
               )
           )
  ), # end of 入库 tab
  
  tabPanel("出库", icon=icon("arrow-circle-up"),
           div(class="layout-container",
               div(class="sticky-sidebar",
                   itemFilterUI(id="outbound_filter", border_color="#28A745", text_color="#28A745", status_choices=c("库存状态"="", "国内入库", "国内出库")),
                   tags$hr(style="margin:5px 0;border:none;"),
                   div(class="card", style="margin-bottom:20px;padding:20px;border:1px solid #007BFF;border-radius:8px;box-shadow:0px 4px 6px rgba(0,0,0,0.1);",
                       tags$h4("出库操作", style="color:#007BFF;font-weight:bold;margin-bottom:15px;"),
                       div(style="display:flex;align-items:center;gap:0px;",
                           textInput("outbound_sku", NULL, placeholder="请扫描条形码操作，并核对物品", width="100%"),
                           actionButton("clear_outbound_sku", label="", icon=icon("xmark", style="color:#D32F2F;"), style="padding:0 5px;border:none;margin-bottom:14px;font-size:18px;background-color:#F5F5F5;height:45px;min-width:34px;")
                       ),
                       checkboxInput("auto_outbound", label="自动出库", value=FALSE),
                       conditionalPanel(condition="input.auto_outbound == true", checkboxInput("speak_outbound_item_name", "念出商品名", value=FALSE)),
                       tags$div(class="card", style="padding:15px;border:2px solid #007BFF;border-radius:8px;background-color:#f9f9f9;box-shadow:0px 4px 6px rgba(0,0,0,0.1);",
                                tags$h4("选择国际运输方式:", style="font-size:18px;font-weight:bold;margin-bottom:15px;"),
                                radioButtons(inputId="outbound_shipping_method", label=NULL, choices=list("空运"="空运", "海运"="海运"), selected="空运", inline=TRUE)
                       ),
                       actionButton("confirm_outbound_btn", "确认出库", icon=icon("check"), class="btn-primary", style="font-size:16px;width:100%;height:42px;margin-top:10px;"),
                       actionButton("revert_outbound_btn", "撤回出库", icon=icon("undo"), class="btn-warning", style="font-size:16px;width:100%;height:42px;margin-top:10px;")
                   )
               ),
               div(class="resizable-divider"),
               div(class="main-panel",
                   div(style="height:300px;margin-bottom:10px;", column(12, uiOutput("outbound_item_info"), style="margin-bottom:10px;")),
                   div(id="item_table_container_outbound", uniqueItemsTableUI("unique_items_table_outbound"))
               )
           )
  ), # end of 出库 tab
  
  tabPanel("售出", icon=icon("dollar-sign"),
           div(class="layout-container",
               div(class="sticky-sidebar",
                   conditionalPanel(condition = "input.sold_tabs == '订单管理'",
                                    div(class="card", style="margin-bottom:5px;padding:15px;border:1px solid #28A745;border-radius:8px;",
                                        tags$h4("订单筛选", style="color:#28A745;font-weight:bold;"),
                                        div(style="display:flex;align-items:center;gap:0px;",
                                            textInput("filter_combined", label=NULL, placeholder="搜索订单信息", width="100%"),
                                            actionButton("clear_filter_combined", label="", icon=icon("xmark", style="color:#D32F2F;"), style="padding:0 5px;border:none;margin-bottom:14px;font-size:18px;background-color:#F5F5F5;height:45px;min-width:34px;")
                                        ),
                                        fluidRow(column(6, selectInput("filter_platform", NULL, choices=c("电商平台"="", "Etsy", "Shopify", "TikTok", "其他"), selected="", width="100%")),
                                                 column(6, selectInput("filter_order_status", NULL, choices=c("订单状态"="", "备货", "预定", "调货", "装箱", "发出", "在途", "送达", "取消"), selected="", width="100%"))),
                                        # fluidRow(column(12, dateRangeInput("filter_order_date", "订单创建时间", start=Sys.Date()-90, end=Sys.Date()+1, format="yyyy-mm-dd", width="100%"))),
                                        fluidRow(
                                          column(12, airDatepickerInput(
                                            inputId = "filter_order_date",
                                            label = "订单创建时间",
                                            range = TRUE,
                                            value = c(Sys.Date() - 90, Sys.Date() + 1),
                                            dateFormat = "yyyy-MM-dd",
                                            width = "100%",
                                            position = "bottom left"  # 控制日期选择器在输入框下方弹出
                                          ))
                                        ),
                                        
                                        fluidRow(column(4, actionButton("delete_order_btn", "删除", class="btn-danger", style="width:100%;")),
                                                 column(4, actionButton("reset_filter_btn", "重置", class="btn-info", style="width:100%;")),
                                                 column(4, actionButton("refresh_orders", "刷新", class="btn-secondary", style="width:100%;")))
                                    )      
                   ),
                   
                   conditionalPanel(condition = "input.sold_tabs == '物品售出'",
                                    itemFilterUI(id="sold_filter", border_color="#28A745", text_color="#28A745", status_choices=c("库存状态"="", "国内入库", "国内出库", "美国入库", "美国调货", "国内售出"))
                   ),
                   
                   tags$hr(style="margin:5px 0;border:none;"),
                   div(id="orderForm", class="card", style="margin-bottom:5px;padding:15px;border:1px solid #007BFF;border-radius:8px;box-shadow:0px 4px 6px rgba(0,0,0,0.1);",
                       div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:15px;",
                           tags$h4("订单登记与更新", style="color:#007BFF;font-weight:bold;margin:0;"),
                           actionButton("clear_order_btn", "清空订单", icon=icon("eraser"), class="btn-warning", style="font-size:16px;height:42px;padding:0 15px;")
                       ),
                       fluidRow(column(7, textInput("order_id", NULL, placeholder="订单号", width="100%")),
                                column(5, selectInput(inputId="platform", NULL, choices=c("电商平台"="", "Etsy"="Etsy", "Shopify"="Shopify", "TikTok"="TikTok", "其他"="其他"), selected="", width="100%"))),
                       fluidRow(column(7, textInput("customer_name", NULL, placeholder="顾客姓名", width="100%")),
                                column(5, textInput("customer_netname", NULL, placeholder="顾客网名", width="100%"))),
                       fluidRow(column(6, numericInput("transaction_amount", NULL, value=NULL, min=0, step=0.01, width="100%")),
                                column(3, checkboxInput("is_transfer_order", "调货", value=FALSE)),
                                column(3, checkboxInput("is_preorder", "预定", value=FALSE))),
                       hidden(div(id="preorder_fields", style="border:1px solid #ccc;padding:10px;margin-bottom:10px;display:flex;flex-direction:column;align-items:center;",
                                  selectizeInput("preorder_supplier", "预定单供应商", choices=NULL, width="100%", options=list(placeholder='填选供应商...')),
                                  textAreaInput(inputId="preorder_item_name", label="预定单商品名（每行一个品名）", placeholder="请输入或选择", width="100%", rows=3),
                                  selectizeInput(inputId="preorder_item_name_db", label=NULL, choices=NULL, width="100%", options=list(placeholder="已有商品名列表")),
                                  div(id="preorder_image_preview", style="width:200px;height:auto;margin-top:10px;text-align:center;",
                                      img(src="", id="preorder_img", style="max-width:100%;max-height:200px;display:none;border:1px solid #ddd;border-radius:8px;")))),
                       div(style="border:1px solid #28A745;border-radius:8px;background-color:#f9f9f9;padding:15px;margin-bottom:10px;",
                           textInput("tracking_number", "运单号", placeholder="输入运单号或运单提取", width="100%"),
                           fileInput("shiplabel_pdf_upload", "上传运单PDF", accept=".pdf", width="100%"), uiOutput("upload_status_message")),
                       imageModuleUI("image_sold", label="订单图片上传", label_color="#007BFF"),
                       textAreaInput("order_notes", NULL, placeholder="请输入备注内容", width="100%", height="100px"),
                       div(style="margin-top:10px;display:flex;flex-direction:column;gap:5px;",
                           div(style="display:flex;", uiOutput("register_order_button_ui", style="width:100%;")),
                           div(style="margin-top:5px;display:flex;justify-content:center;",
                               actionButton("merge_order_btn", "合并订单", icon=icon("object-group"), class="btn-primary", style="font-size:16px;width:100%;height:42px;")))
                   )
               ),
               div(class="resizable-divider"),
               div(class="main-panel",
                   tabsetPanel(id="sold_tabs", type="pills",
                               tabPanel(title="订单管理",
                                        div(class="card", style="height:460px;padding:5px;border:1px solid #ccc;border-radius:8px;", orderTableUI("orders_table_module")),
                                        div(class="card", style="padding:5px;border:1px solid #ccc;border-radius:8px;", uiOutput("associated_items_title"), uiOutput("order_items_cards"))),
                               tabPanel(title="物品售出",
                                        fluidRow(
                                          column(6, div(class="card", style="padding:10px;margin-bottom:5px;border:1px solid #007BFF;border-radius:10px;box-shadow:0 4px 6px rgba(0,0,0,0.1);",
                                                        div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:0px;height:30px;padding:0px",
                                                            tags$h4(HTML(paste0(as.character(icon("warehouse")), "  货架  ", span(style="display:inline-flex;color:#007BFF;font-size:18px;", textOutput("shelf_count")))), style="color:#007BFF;font-weight:bold;"),
                                                            div(style="display:flex;",
                                                                tags$label("排序:", style="margin-right:10px;font-weight:bold;font-size:14px;"),
                                                                radioButtons(inputId="arrow_direction", label=NULL, choices=list("↑"="up", "↓"="down"), selected="up", inline=TRUE),
                                                                tags$style(HTML("#arrow_direction.form-group{margin-bottom:0 !important;} #arrow_direction .radio-inline{margin-right:10px;}"))),
                                                            textInput(inputId="sku_to_shelf", label=NULL, placeholder="扫码上架", width="200px"),
                                                            tags$style(HTML("#sku_to_shelf{height:35px !important;font-size:15px;padding:5px;}"))),
                                                        DTOutput("shelf_table"))),
                                          column(6, div(class="card", style="padding:10px;margin-bottom:5px;border:1px solid #28A745;border-radius:10px;box-shadow:0 4px 6px rgba(0,0,0,0.1);",
                                                        div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:0px;height:30px",
                                                            tags$h4(HTML(paste0(as.character(icon("box")), "  发货箱  ", span(style="display:inline-flex;color:#28A745;font-size:18px;", textOutput("box_count")))), style="color:#28A745;font-weight:bold;margin:0;"),
                                                            textInput(inputId="sku_to_box", label=NULL, placeholder="扫码入箱", width="200px"),
                                                            tags$style(HTML("#sku_to_box{height:35px !important;font-size:15px;padding:5px;}"))),
                                                        DTOutput("box_table"),
                                                        fluidRow(
                                                          column(6, actionButton("confirm_order_btn", "确认售出", icon=icon("check"), class="btn-primary", style="font-size:16px;width:100%;height:50px;margin-top:10px;")),
                                                          column(6, tags$div(style="display:flex;align-items:center;justify-content:flex-start;border:1px solid #007BFF;border-radius:8px;height:50px;padding:0 10px;margin-top:10px;",
                                                                             tags$span("国际运输:", style="font-size:16px;font-weight:bold;margin-right:15px;line-height:1;"),
                                                                             tags$div(style="display:flex;align-items:center;height:100%;margin-bottom:0;",
                                                                                      tags$style(HTML("#sold_shipping_method .radio{margin-bottom:0 !important;} #sold_shipping_method{margin-bottom:0 !important;}")),
                                                                                      radioButtons(inputId="sold_shipping_method", label=NULL, choices=list("空运"="空运", "海运"="海运"), selected="空运", inline=TRUE)))))))),
                                        tags$hr(style="margin:5px 0;border:1px solid #ddd;"),
                                        div(id="item_table_container_sold", uniqueItemsTableUI("unique_items_table_sold")))
                   )
               )
           )
  ), # End of 售出
  
  tabPanel(
    "物品管理", icon = icon("list-check"),
    div(class = "layout-container",
        div(class = "sticky-sidebar",
            itemFilterUI(id = "manage_filter", border_color = "#28A745", text_color = "#28A745", use_purchase_date = TRUE),
            tags$hr(),
            tabsetPanel(id = "manage_tabs", type = "pills",
                        tabPanel("更新图片", icon = icon("image"),
                                 div(class = "card shadow-sm", style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
                                     tags$p("请点选一行（一种商品）进行图片更新。", style = "font-size: 14px; color: #6c757d; margin-bottom: 5px;"),
                                     imageModuleUI("image_manage", label = "更新商品图片"),
                                     actionButton("update_image_btn", "更新图片", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;")
                                 )
                        ),
                        tabPanel("更新信息", icon = icon("edit"),
                                 div(class = "card shadow-sm", style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
                                     tags$p("请点选一行或多行进行信息更新。", style = "font-size: 14px; color: #6c757d; margin-bottom: 5px;"),
                                     fluidRow(
                                       column(12, numericInput("update_product_cost", "修改单价", value = NULL, min = 0, width = "100%")),
                                       column(12, numericInput("update_shipping_cost", "修改国内运费", value = NULL, min = 0, width = "100%")),
                                       column(12, dateInput("update_purchase_date", "修改采购日期", value = Sys.Date(), width = "100%"))
                                     ),
                                     fluidRow(
                                       column(6, actionButton("update_info_btn", "更新信息", icon = icon("pen"), style = "background-color: #006400; color: white; width: 100%;")),
                                       column(6, actionButton("clear_info_btn", "清空", icon = icon("eraser"), style = "background-color: #8B0000; color: white; width: 100%;"))
                                     )
                                 )
                        ),
                        tabPanel("删除", icon = icon("trash"),
                                 div(class = "card shadow-sm", style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
                                     tags$p("请点选一行或多行物品，支持批量删除。", style = "font-size: 14px; color: #6c757d; margin-bottom: 5px;"),
                                     actionButton("confirm_delete_btn", "确认删除", icon = icon("check"), class = "btn-primary", style = "font-size: 16px; width: 100%; height: 42px;")
                                 )
                        )
            )
        ),
        div(class = "resizable-divider"),
        div(class = "main-panel",
            div(id = "item_table_container_manage",
                uniqueItemsTableUI("unique_items_table_manage")
            )
        )
    )
  ), # end of 物品管理 tab
  
  tabPanel("瑕疵品管理", icon=icon("exclamation-circle"),
           div(class="layout-container",
               div(class="sticky-sidebar",
                   itemFilterUI(id="defect_filter", border_color="#28A745", text_color="#28A745", use_status=FALSE, use_purchase_date=TRUE),
                   tags$hr(),
                   div(style="margin-bottom:20px;", actionButton("register_defective", "登记瑕疵品", icon=icon("circle-exclamation"), class="btn-warning", style="font-size:16px;width:100%;")),
                   div(style="margin-bottom:20px;", tags$label("仅显示无瑕品", class="control-label"), switchInput(inputId="show_perfects_only", label=NULL, value=FALSE)),
                   tags$hr(),
                   div(style="margin-bottom:20px;", actionButton("register_repair", "登记修复品", icon=icon("hammer"), class="btn-success", style="font-size:16px;width:100%;")),
                   div(style="margin-bottom:20px;", tags$label("仅显示瑕疵品", class="control-label"), switchInput(inputId="show_defects_only", label=NULL, value=FALSE)),
                   tags$hr(),
                   textAreaInput(inputId="manage_defective_notes", label="备注：", placeholder="请输入备注内容...", width="100%")
               ),
               div(class="resizable-divider"),
               div(class="main-panel",
                   div(id="item_table_container_defect", uniqueItemsTableUI("unique_items_table_defect"))
               )
           )
  ), # end of 瑕疵品管理 tab
  
  tabPanel("国际物流管理", icon=icon("globe"),
           div(class="layout-container",
               div(class="sticky-sidebar",
                   itemFilterUI(id="logistic_filter", use_purchase_date=FALSE, use_sold_date=TRUE, use_exit_date=TRUE, use_status=FALSE, border_color="#28A745", text_color="#28A745"),
                   tags$hr(style="margin:5px 0;border:none;"),
                   tabsetPanel(id="intl_shipment_tabs", type="pills",
                               tabPanel(title=tagList(icon("file-alt"), "登记国际运单"), value="register_shipment",
                                        div(class="card shadow-sm", style="padding:10px;border:1px solid #007BFF;border-radius:8px;box-shadow:0px 4px 6px rgba(0,0,0,0.1);",
                                            tags$h4("登记国际运单", style="color:#007BFF;font-weight:bold;margin-bottom:15px;"),
                                            fluidRow(
                                              column(12, textInput("intl_tracking_number", "国际运单号:", placeholder="请输入空运或海运运单号", width="100%")),
                                              column(12, textOutput("intl_status_display"), style="color:blue;font-weight:bold;margin-bottom:20px;"),
                                              column(12, selectInput("intl_shipping_method", "国际运输方式:", choices=c("空运"="空运", "海运"="海运"), selected="空运", width="100%")),
                                              column(12, numericInput("intl_total_shipping_cost", "国际物流总运费 (元):", value=0, min=0, width="100%"))),
                                            fluidRow(
                                              column(6, actionButton("register_shipment_btn", "登记运单", icon=icon("save"), class="btn-primary", style="margin-top:20px;width:100%;font-size:16px;")),
                                              column(6, actionButton("batch_value_btn", "包裹货值", icon=icon("dollar-sign"), class="btn-success", style="margin-top:20px;width:100%;font-size:16px;"))),
                                            fluidRow(
                                              column(6, actionButton("delete_shipment_btn", "删除运单", icon=icon("trash"), class="btn-danger", style="margin-top:20px;width:100%;font-size:16px;")),
                                              column(6, actionButton("clean_shipment_btn", "清空填写", icon=icon("trash"), class="btn-info", style="margin-top:20px;width:100%;font-size:16px;")))
                                        )
                               ),
                               tabPanel(title=tagList(icon("link"), "挂靠管理"), value="link_management",
                                        div(class="card shadow-sm", style="padding:10px;border:1px solid #28A745;border-radius:8px;box-shadow:0px 4px 6px rgba(0,0,0,0.1);",
                                            tags$h4("挂靠管理", style="color:#28A745;font-weight:bold;margin-bottom:15px;"),
                                            fluidRow(
                                              column(12, textInput("intl_link_tracking_number", "", placeholder="请输入要挂靠的运单号", width="100%")),
                                              column(12, htmlOutput("intl_link_display"), style="color:blue;font-weight:bold;margin-bottom:20px;")),
                                            fluidRow(
                                              column(6, actionButton("link_tracking_btn", "挂靠运单", icon=icon("link"), class="btn-primary", style="margin-top:20px;width:100%;", disabled=TRUE)),
                                              column(6, actionButton("unlink_tracking_btn", "解除挂靠", icon=icon("link-slash"), class="btn-danger", style="margin-top:20px;width:100%;", disabled=TRUE)))
                                        )
                               )
                   )
               ),
               div(class="resizable-divider"),
               div(class="main-panel",
                   div(id="item_table_container_logistics", uniqueItemsTableUI("unique_items_table_logistics"))
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
          id = "sidebar_tabs",
          type = "pills",
          selected = "账务登记",
          tabPanel(
            title = "账务登记", icon = icon("file-invoice-dollar"),
            tags$h4("账务登记", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
            fluidRow(
              column(7, numericInput("amount", "金额:", value = 0, min = 0, width = "100%")),
              column(5, radioButtons(inputId = "transaction_type", label = "交易类型:", choices = c("转出" = "out", "转入" = "in"), selected = NULL, inline = FALSE))
            ),
            fluidRow(
              column(12, dateInput("custom_date", "转款日期:", value = Sys.Date(), width = "100%")),
              column(12, timeInput("custom_time", "转款时间:", value = format(Sys.time(), "%H:%M:%S"), width = "100%"))
            ),
            fluidRow(
              column(12, selectInput(inputId = "transaction_category", label = "转账类别:", choices = c("采购", "税费", "杂费", "工资", "债务", "社保", "图解", "其他"), selected = "其他", width = "100%")),
              div(style = "margin-top: 8px; padding: 10px; background-color: #f9f9f9; margin-bottom: 10px; border: 1px solid #ddd; border-radius: 4px; font-size: 14px; color: #555;", textOutput("transaction_category_note", inline = TRUE))
            ),
            textAreaInput("remarks", "备注:", placeholder = "请输入备注内容", width = "100%"),
            imageModuleUI("image_transactions", label = "转账证据上传", label_color = "#007BFF"),
            actionButton("record_transaction", "登记", icon = icon("save"), class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            fluidRow(
              column(6, actionButton("delete_transaction", "删除选中行", icon = icon("trash"), class = "btn-danger", style = "width: 100%;")),
              column(6, actionButton("reset_form", "重置", icon = icon("redo"), class = "btn-info", style = "width: 100%;"))
            )
          ),
          tabPanel(
            title = "资金转移", icon = icon("exchange-alt"),
            tags$h4("资金转移", style = "color: #28A745; font-weight: bold; margin-bottom: 15px;"),
            numericInput("transfer_amount", "转移金额:", value = 0, min = 0, width = "100%"),
            selectInput(inputId = "from_account", label = "转出账户:", choices = c("工资卡", "美元卡", "买货卡", "一般户卡"), selected = "美元卡", width = "100%"),
            selectInput(inputId = "to_account", label = "转入账户:", choices = c("工资卡", "美元卡", "买货卡", "一般户卡"), selected = NULL, width = "100%"),
            fluidRow(
              column(12, selectInput(inputId = "transfer_category", label = "转账类别:", choices = c("采购", "税费", "杂费", "工资", "债务", "社保", "其他"), selected = "其他", width = "100%")),
              div(style = "margin-top: 8px; padding: 10px; background-color: #f9f9f9; margin-bottom: 10px; border: 1px solid #ddd; border-radius: 4px; font-size: 14px; color: #555;", textOutput("transfer_category_note", inline = TRUE))
            ),
            textAreaInput("transfer_remarks", "备注:", placeholder = "请输入备注内容", width = "100%"),
            imageModuleUI("image_transfer", label = "转账证据上传", label_color = "#28A745"),
            actionButton("record_transfer", "记录转移", icon = icon("exchange-alt"), class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
            fluidRow(
              column(6, actionButton("delete_transfer", "删除选中行", icon = icon("trash"), class = "btn-danger", style = "width: 100%;")),
              column(6, actionButton("reset_form_transfer", "重置", icon = icon("redo"), class = "btn-info", style = "width: 100%;"))
            )
          ),
          tabPanel(
            title = "汇总", icon = icon("chart-bar"),
            tags$h4("统计汇总", style = "color: #17A2B8; font-weight: bold; margin-bottom: 15px;"),
            dateRangeInput("summary_date_range", "选择统计时间范围:", start = Sys.Date() - 365, end = Sys.Date() + 1, width = "100%"),
            fluidRow(
              column(4, actionButton("summary_daily", "按天", class = "btn-primary", style = "width: 100%;")),
              column(4, actionButton("summary_monthly", "按月", class = "btn-success", style = "width: 100%;")),
              column(4, actionButton("summary_yearly", "按年", class = "btn-warning", style = "width: 100%;"))
            )
          )
        )
      ),
      div(class = "resizable-divider"),
      div(
        class = "main-panel",
        tabsetPanel(
          id = "transaction_tabs",
          type = "pills",
          tabPanel("账户余额总览",
                   fluidRow(
                     column(12, div(
                       class = "card shadow-lg",
                       style = "background: #1F1F1F; color: white; padding: 40px; text-align: center; border-radius: 16px; margin-top: 20px; margin-bottom: 40px; border: 2px solid #FFC107;",
                       tags$h4("总余额", style = "font-weight: bold; font-size: 30px; margin-bottom: 20px; letter-spacing: 1.5px;"),
                       tags$h3(textOutput("total_balance"), style = "font-size: 40px; margin-top: 0; font-weight: bold; text-shadow: 2px 2px 4px rgba(255, 193, 7, 0.8); color: #FFC107;")
                     ))
                   ),
                   fluidRow(lapply(accounts, function(acc) {
                     column(3, div(
                       class = "card shadow-lg",
                       style = sprintf("background: %s; color: white; padding: 20px; text-align: center; border-radius: 16px; position: relative; overflow: hidden;", acc$gradient),
                       tags$div(
                         style = "position: absolute; top: -10px; left: -10px; opacity: 0.3;",
                         tags$img(src = "https://dummyimage.com/100x100/fff/000.png&text=$", width = "60px", height = "60px")
                       ),
                       tags$h4(acc$name, style = "font-weight: bold; margin-bottom: 10px;"),
                       tags$h3(textOutput(acc$outputId), style = "font-size: 24px; margin-top: 0;")
                     ))
                   }))
          ),
          tabPanel(title = "买货卡(139)", value = "买货卡", DTOutput("purchase_card_table")),
          tabPanel(title = "一般户卡(541)", value = "一般户卡", DTOutput("general_card_table")),
          tabPanel(title = "工资卡(567)", value = "工资卡", DTOutput("salary_card_table")),
          tabPanel(title = "美元卡(553)", value = "美元卡", DTOutput("dollar_card_table"))
        )
      )
    )
  ), # End of 账务管理
  
  tabPanel(
    "员工管理", icon = icon("users"),
    div(class = "layout-container",
        div(class = "sticky-sidebar",
            tabsetPanel(
              id = "employee_tabs",
              type = "pills",
              # 员工考勤分页
              tabPanel(
                "员工考勤", icon = icon("clock"),
                tags$hr(),
                selectInput("attendance_employee_name", "选择员工:", choices = NULL, width = "100%"),
                actionButton("generate_attendance_report", "考勤汇总月表", icon = icon("table"), 
                             class = "btn-primary", style = "width: 100%; margin-top: 10px;"),
                div(
                  class = "card",
                  style = "margin: 15px; padding: 10px; background-color: #f9f9f9; border-radius: 5px; border: 1px solid #ddd;",
                  h4("当前工作中员工", style = "margin-bottom: 15px; text-align: center;"),
                  uiOutput("current_employees_ui")
                )
              ),
              # 新增考勤编辑分页
              tabPanel(
                "考勤编辑", icon = icon("edit"),
                div(
                  class = "card shadow-sm",
                  style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9; margin-bottom: 15px;",
                  tags$h4("添加/修改考勤记录", style = "color: #28A745; margin-bottom: 10px;"),
                  selectInput("edit_attendance_employee", "选择员工:", choices = NULL, width = "100%"),
                  selectInput("edit_attendance_work_type", "工作类型:", choices = c("直播", "采购"), width = "100%"),
                  textInput("edit_attendance_clock_in", "上班时间 (YYYY-MM-DD HH:MM:SS):", value = "", width = "100%"),
                  textInput("edit_attendance_clock_out", "下班时间 (YYYY-MM-DD HH:MM:SS):", value = "", placeholder = "留空表示未结束", width = "100%"),
                  numericInput("edit_attendance_sales_amount", "销售额 ($):", value = 0, min = 0, step = 0.01, width = "100%"),
                  textInput("edit_attendance_remark", "备注:", value = "", width = "100%"),
                  actionButton("add_attendance_btn", "添加", icon = icon("plus"), class = "btn-success", 
                               style = "width: 30%; margin-top: 10px; margin-right: 2%;"),
                  actionButton("update_attendance_btn", "修改", icon = icon("edit"), class = "btn-primary", 
                               style = "width: 30%; margin-top: 10px; margin-right: 2%;"),
                  actionButton("delete_attendance_btn", "删除", icon = icon("trash"), class = "btn-danger", 
                               style = "width: 30%; margin-top: 10px;")
                )
              ),
              # 员工管理分页
              tabPanel(
                "员工管理", icon = icon("user"),
                div(
                  class = "card shadow-sm",
                  style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9; margin-bottom: 15px;",
                  tags$h4("添加新员工", style = "color: #28A745; margin-bottom: 10px;"),
                  textInput("new_employee_name", NULL, placeholder = "请输入员工姓名", width = "100%"),
                  actionButton("add_employee_btn", "添加员工", icon = icon("plus"), class = "btn-success", 
                               style = "width: 100%; margin-top: 10px;")
                ),
                div(
                  class = "card shadow-sm",
                  style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9; margin-bottom: 15px;",
                  tags$h4("设置员工薪酬", style = "color: #007BFF; margin-bottom: 10px;"),
                  selectInput("edit_employee_name", NULL, choices = NULL, width = "100%"),
                  numericInput("edit_live_rate", "直播时薪 (¥/小时):", value = 0, min = 0, step = 0.01, width = "100%"),
                  numericInput("edit_purchase_rate", "采购时薪 (¥/小时):", value = 0, min = 0, step = 0.01, width = "100%"),
                  actionButton("update_employee_btn", "更新薪酬", icon = icon("edit"), class = "btn-primary", 
                               style = "width: 100%; margin-top: 10px;")
                ),
                div(
                  class = "card shadow-sm",
                  style = "border: 1px solid #e0e0e0; border-radius: 8px; padding: 10px; background-color: #f9f9f9;",
                  tags$h4("删除员工", style = "color: #DC3545; margin-bottom: 10px;"),
                  selectInput("delete_employee_name", NULL, choices = NULL, width = "100%"),
                  actionButton("delete_employee_btn", "删除员工", icon = icon("trash"), class = "btn-danger", 
                               style = "width: 100%; margin-top: 10px;")
                )
              )
            )
        ),
        div(class = "resizable-divider"),
        div(class = "main-panel",
            conditionalPanel(
              condition = "input.employee_tabs == '员工考勤'",
              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                dateRangeInput(
                  inputId = "plot_date_range",
                  label = NULL, # 不显示标签
                  start = Sys.Date() - 7, # 默认开始日期为过去一周
                  end = Sys.Date(),       # 默认结束日期为今天
                  width = "40%"           # 控制宽度
                ),
                radioButtons(
                  inputId = "employee_work_plot_type",
                  label = NULL, # 不显示标签
                  choices = c(
                    "工作时长 (小时)" = "hours",
                    "薪酬 (¥)" = "pay",
                    "直播销售额 ($)" = "sales"
                  ),
                  selected = "hours",
                  inline = TRUE # 按钮横向排列
                )
              ),
              plotlyOutput("employee_work_plot", height = "550px")
            ),
            conditionalPanel(
              condition = "input.employee_tabs == '考勤编辑'",
              DTOutput("attendance_table")
            )
        )
    )
  ), # End of 员工管理
  
  tabPanel(
    "查询", icon = icon("search"),
    div(class = "layout-container",
        div(class = "sticky-sidebar",
            conditionalPanel(
              condition = "input.query_tabs == '商品状态'",
              div(
                itemFilterUI(id = "query_filter", border_color = "#28A745", text_color = "#28A745", use_status = FALSE, use_purchase_date = FALSE),
                tags$hr(),
                div(
                  class = "card",
                  style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("查询商品", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
                  textInput("query_sku", NULL, placeholder = "请扫描或输入SKU", width = "100%"),
                  actionButton("clear_query_sku_btn", "清空", icon = icon("eraser"), class = "btn btn-warning")
                ),
                div(
                  class = "card",
                  style = "margin-bottom: 20px; padding: 20px; border: 1px solid #DC3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                  tags$h4("售罄物品", style = "color: #DC3545; font-weight: bold; margin-bottom: 15px;"),
                  radioButtons(
                    inputId = "query_stock_status",
                    label = NULL,
                    choices = c("不过滤" = "none", "美国售罄, 国内有货" = "us", "国内售罄, 美国有货" = "domestic", "全库存售罄" = "all"),
                    selected = "none",
                    inline = FALSE
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "input.query_tabs == '采购开销'",
              div(
                class = "card",
                style = "margin-bottom: 20px; padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                tags$h4("供应商筛选", style = "color: #007BFF; font-weight: bold; margin-bottom: 15px;"),
                selectInput(
                  inputId = "purchase_check_filter_maker",
                  label = NULL,
                  choices = c("所有供应商" = "all"),  # 初始默认选项          
                  selected = "all",
                  width = "100%"
                )
              )
            ),
            
            conditionalPanel(
              condition = "input.query_tabs == '库存总览'",
              div(
                p("此处留空，未来扩展...", style = "color: #888; text-align: center; margin-top: 20px;")
              )
            )
        ),
        div(class = "resizable-divider"),
        div(class = "main-panel",
            tabsetPanel(id = "query_tabs", type = "pills",
                        tabPanel("商品状态",
                                 fluidRow(
                                   column(5,
                                          div(class = "card", style = "height: 373px; margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("商品信息", style = "color: #007BFF; font-weight: bold; padding-left: 10px;"),
                                              uiOutput("query_item_info")
                                          )
                                   ),
                                   column(4,
                                          div(class = "card", style = "margin-bottom: 5px; padding: 5px; border: 1px solid #28a745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("库存状态", style = "color: #28a745; font-weight: bold; padding-left: 10px;"),
                                              plotlyOutput("inventory_status_chart", height = "320px")
                                          )
                                   ),
                                   column(3,
                                          div(class = "card", style = "margin-bottom: 5px; padding: 5px; border: 1px solid #dc3545; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("瑕疵情况", style = "color: #dc3545; font-weight: bold; padding-left: 10px"),
                                              plotlyOutput("defect_status_chart", height = "320px")
                                          )
                                   )
                                 ),
                                 div(style = "display: flex; flex-direction: column;",
                                     div(style = "flex-grow: 1; overflow-y: auto; padding-top: 10px;",
                                         div(id = "context-menu", style = "display: none; position: absolute; background: white; border: 1px solid #ccc; box-shadow: 0px 4px 6px rgba(0,0,0,0.2); padding: 5px; border-radius: 5px; z-index: 1000;",
                                             actionButton("query_purchase_request", "采购请求", class = "btn btn-primary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
                                             uiOutput("query_outbound_request_btn")
                                         ),
                                         div(id = "inventory_table_container_query",
                                             DTOutput("filtered_inventory_table_query")
                                         )
                                     )
                                 )
                        ),
                        tabPanel("采购开销",
                                 fluidRow(
                                   column(12,
                                          div(class = "card", style = "margin-bottom: 5px; padding: 5px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              fluidRow(
                                                column(3, dateRangeInput("time_range", label = "选择采购时间范围", start = Sys.Date() - 30, end = Sys.Date() + 1)),
                                                column(3, radioButtons("precision", label = "选择统计精度", choices = c("天" = "天", "周" = "周", "月" = "月", "年" = "年"), selected = "天", inline = TRUE)),
                                                column(5, radioButtons("expense_type", label = "选择显示内容", choices = c("成本+国内运费" = "cost_domestic", "成本" = "cost", "国内运费" = "domestic_shipping", "国际运费" = "intl_shipping", "总开销" = "total"), selected = "cost_domestic", inline = TRUE)),
                                                column(1, actionButton("reset_time_range", label = "", icon = icon("redo"), class = "btn-warning", style = "height: 50px; font-size: 14px;"))
                                              ),
                                              fluidRow(
                                                column(9, plotlyOutput("expense_chart", height = "350px")),
                                                column(3, plotlyOutput("pie_chart", height = "350px"))
                                              ),
                                              uiOutput("confirm_expense_check_ui"),
                                              uiOutput("purchase_summary_by_maker_ui")
                                          )
                                   )
                                 )
                        ),
                        tabPanel("库存总览",
                                 fluidRow(
                                   column(3,
                                          div(class = "card", style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("国内库存", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h3(textOutput("domestic_total_count"), style = "color: #007BFF; font-weight: bold;"), tags$p("物品总数")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("domestic_total_value"), style = "color: #007BFF;"), tags$p("货物价值")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("domestic_shipping_cost"), style = "color: #007BFF;"), tags$p("运输成本"))
                                          )
                                   ),
                                   column(3,
                                          div(class = "card", style = "padding: 20px; border: 1px solid #28A745; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("国际物流", style = "color: #28A745; font-weight: bold; text-align: center;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h3(textOutput("logistics_total_count"), style = "color: #28A745; font-weight: bold;"), tags$p("物品总数")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("logistics_total_value"), style = "color: #28A745;"), tags$p("货物价值")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("logistics_shipping_cost"), style = "color: #28A745;"), tags$p("运输成本"))
                                          )
                                   ),
                                   column(3,
                                          div(class = "card", style = "padding: 20px; border: 1px solid #6F42C1; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("美国库存", style = "color: #6F42C1; font-weight: bold; text-align: center;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h3(textOutput("us_total_count"), style = "color: #6F42C1; font-weight: bold;"), tags$p("物品总数")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("us_total_value"), style = "color: #6F42C1;"), tags$p("货物价值")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("us_shipping_cost"), style = "color: #6F42C1;"), tags$p("运输成本"))
                                          )
                                   ),
                                   column(3,
                                          div(class = "card", style = "padding: 20px; border: 1px solid #FF5733; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("商品售出", style = "color: #FF5733; font-weight: bold; text-align: center;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h3(textOutput("sold_total_count_with_shipping"), style = "color: #FF5733; font-weight: bold;"), tags$p("物品总数（已送达）")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("sold_total_value"), style = "color: #FF5733;"), tags$p("货物价值")),
                                              tags$hr(style = "border: none; height: 1px; background-color: #ddd; margin: 15px 0;"),
                                              div(style = "text-align: center; margin-top: 10px;", tags$h4(textOutput("sold_shipping_cost"), style = "color: #FF5733;"), tags$p("运输成本"))
                                          )
                                   )
                                 ),
                                 tags$hr(style = "margin: 10px 0; border: 1px solid #ddd;"),
                                 fluidRow(
                                   column(12,
                                          div(class = "card", style = "padding: 20px; border: 1px solid #007BFF; border-radius: 8px; box-shadow: 0px 4px 6px rgba(0,0,0,0.1);",
                                              tags$h4("库存状态流转桑基图", style = "color: #007BFF; font-weight: bold; text-align: center;"),
                                              sankeyNetworkOutput("status_sankey", height = "345px")
                                          )
                                   )
                                 )
                        )
            )
        )
    )
  ), # end of 查询 tab
  
  tabPanel("数据下载", icon=icon("download"),
           div(class="layout-container",
               div(class="sticky-sidebar",
                   div(class="card shadow-sm", style="border:1px solid #e0e0e0;border-radius:8px;padding:20px;background-color:#f9f9f9;",
                       tags$h4("表格筛选", style="color:#007BFF;font-weight:bold;margin-bottom:15px;"),
                       uiOutput("download_maker_ui"),
                       selectizeInput(inputId="download_item_name", label=NULL, choices=NULL, selected=NULL, multiple=FALSE, options=list(placeholder="请输入商品名称...", create=FALSE), width="100%"),
                       textInput("download_sku", label = NULL, placeholder = "SKU", width = "100%"),
                       dateRangeInput(inputId="download_date_range", label="选择采购日期范围:", start=Sys.Date()-365, end=Sys.Date()+1, format="yyyy-mm-dd", separator=" 至 ", width="100%"),
                       actionButton("download_reset_filters", "重置筛选", class="btn-secondary")),
                   tags$hr(),
                   downloadButton(outputId="download_summary_xlsx", label="下载物品汇总表（按采购日期）", class="btn-primary", style="width:100%;margin-top:10px;"),
                   downloadButton(outputId="download_details_xlsx", label="下载物品明细表", class="btn-primary", style="width:100%;margin-top:10px;")
               ),
               div(class="resizable-divider"),
               div(class="main-panel", uniqueItemsTableUI("unique_items_table_download"))
           )
  ) # End of 数据下载 tab
)
