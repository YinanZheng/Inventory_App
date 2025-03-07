# 定义 UI 函数
ui <- secure_app(
  ui = function(request) {
    navbarPage(
      title = "ERP系统（国内端）",
      id = "inventory_cn",  # 设置 ID，用于监听当前选中的主页面
      theme = shinytheme("flatly"), # 可选主题
      position = "fixed-top",
      
      header = tagList(
        useShinyjs(),  # 启用 shinyjs
        
        # 物品表刷新（联动刷新库存表与订单表）
        actionButton(
          "refresh_global_items_btn",
          "",
          icon = icon("sync"),
          class = "btn-success",
          style = "position: fixed; top: 8px; right: 20px; z-index: 9999;"
        ),
        
        # 加载动画界面
        tags$div(
          id = "loading-screen",
          style = "position: fixed; width: 100%; height: 100%; background: white; 
           z-index: 9999; display: flex; flex-direction: column; 
           justify-content: center; align-items: center; text-align: center;",
          
          # 旋转的毛线球 GIF
          tags$img(src = "https://www.goldenbeanllc.com/icons/spinning_yarn.gif", 
                   style = "width: 80px; height: 80px;"),
          
          # 加载提示文字
          tags$p("系统加载中，请稍后...", 
                 style = "font-size: 18px; font-weight: bold; color: #333; margin-top: 10px;")
        ),
        
        # 库存状态浮动框 （协作页）
        tags$div(
          id = "inventory-status-popup",
          style = "display: none; position: absolute; z-index: 9999; background: white; border: 1px solid #ccc; padding: 5px; box-shadow: 2px 2px 8px rgba(0,0,0,0.2); border-radius: 5px; min-width: 220px; min-height: 220px;",
          plotlyOutput("colab_inventory_status_chart", width = "220px", height = "220px")
        ),
        
        tags$head(
          tags$link(rel = "icon", type = "image/x-icon", href = "https://www.goldenbeanllc.com/icons/favicon-96x96.png"),
          
          tags$style(HTML("
      
      /* 强制导航栏支持水平滚动 */
      .navbar-nav {
        display: flex !important;
        flex-wrap: nowrap !important;
        overflow-x: auto !important;
        white-space: nowrap !important;
        max-width: 100% !important; /* 防止宽度限制 */
      }
      
      /* 导航栏滚动条样式 */
      .navbar-nav::-webkit-scrollbar {
        height: 6px;
      }
      .navbar-nav::-webkit-scrollbar-thumb {
        background: #007BFF;
        border-radius: 10px;
      }
      
      /* 强制显示滚动条并隐藏标题 */
      @media (max-width: 1470px) {
        .navbar-nav {
          overflow-x: scroll !important;
        }
        .navbar-brand {
          display: none !important; /* 隐藏标题 */
        }
      }
      
      /* 限制 .navbar 的宽度扩展 */
      .navbar {
        display: block !important;
        overflow: hidden !important;
        width: 100% !important;
      }
      
      /* 小屏幕调整字体和间距 */
      @media (max-width: 950px) {
        .navbar-nav > li > a {
          font-size: 12px !important;
          padding: 6px 8px !important;
        }
      }
      
      /* 为导航栏顶部留出空间 */
      body {
        padding-top: 70px !important;
      }
      
      /* --------------------------------------------------------- */

      /* Flexbox 容器 */
      .layout-container {
        display: flex;
        flex-direction: row;
        height: 100%;
        width: 100%;
        overflow: visible;
      }
      
      .sticky-sidebar {
        position: sticky; /* 保持固定 */
        top: 70px; /* 顶部距离 */
        z-index: 900;
        flex: 0 0 auto; /* 固定宽度并防止被压缩 */
        width: 380px; /* 默认宽度 */
        min-width: 280px; /* 最小宽度 */
        max-width: 580px; /* 最大宽度 */
        height: calc(100vh - 70px); /* 自动计算高度 */
        overflow-y: auto; /* 滚动支持 */
        border-right: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 20px;
        background-color: #f9f9f9;
        transition: width 0.2s ease; /* 增加平滑过渡效果 */
      }
      
      .main-panel {
        flex-grow: 1;
        overflow: auto;
        padding: 20px;
        padding-top: 0px;
        background-color: #ffffff;
        transition: width 0.2s ease; /* 增加平滑过渡效果 */
      }
      
      .resizable-divider {
        background-color: #aaa;
        width: 5px;
        cursor: ew-resize;
        flex-shrink: 0;
      }
    
      table.dataTable thead th {
        white-space: nowrap; /* 表头内容强制不换行 */
      } 
      
      /* DT 搜索框左对齐 */
      div.dataTables_wrapper div.dataTables_filter {
          text-align: left !important; /* 搜索框文字左对齐 */
          float: left !important;      /* 搜索框容器浮动到左侧 */
      }
      
      div.dataTables_wrapper div.dataTables_filter label {
        display: inline-flex;       /* 让标签和输入框同行 */
        align-items: center;       /* 垂直居中对齐 */
        gap: 5px;                  /* 间距调整 */
      }
      
      /* 采购流程链条箭头 */
      .arrow-icon {
        margin-right: 10px;
      }
      
      /* 预定单采购备忘小icon */
      .status-badge {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 6px;
        font-size: 12px;
        font-weight: bold;
        color: white;
        text-align: center;
        margin-left: 10px;
        min-width: 24px;
      }
      .status-existing {
        background-color: #28A745; /* 绿色 */
      }
      .status-new {
        background-color: #FFA500; /* 橙色 */
      }
      
      .note-card {
        display: flex !important;
        opacity: 1 !important;
      }
      .pagination-controls {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-top: 20px;
        justify-content: center;
      }
    ")),
          
          tags$script(HTML("
      // 系统加载画面淡出
      $(document).ready(function() {
        $('#loading-screen').css('transition', 'opacity 1s ease-out');
      });
      
      // 导航栏右上角刷新物品表
      $(document).ready(function() {
        $('#refresh_global_items_btn').on('click', function() {
          Shiny.setInputValue('refresh_item_table', new Date().getTime(), {priority: 'event'});
        });
      });
    
      // 协作页鼠标悬停显示库存状态
      let inventoryStatusTimeout;  // 用于存储定时器 ID
      
      function showInventoryStatus(event, sku) {
        clearTimeout(inventoryStatusTimeout);
      
        // 更新 Shiny 变量，确保服务器端能接收到
        Shiny.setInputValue('hover_sku', sku, {priority: 'event'});
      
        inventoryStatusTimeout = setTimeout(function () {
          var popup = document.getElementById('inventory-status-popup');
      
          if (sku === 'New-Request') {
            popup.style.display = 'none';
          } else {
            popup.style.display = 'block';
            popup.style.position = 'absolute';
            popup.style.left = (event.pageX + 20) + 'px';
            popup.style.top = (event.pageY + 20) + 'px';
          }
        }, 1000);  // 1秒后执行
      }
      
      function hideInventoryStatus() {
        clearTimeout(inventoryStatusTimeout);
        document.getElementById('inventory-status-popup').style.display = 'none';
      }

      // 复制粘贴图片
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
      });

      // JavaScript 实现分隔条拖拽
      document.addEventListener('DOMContentLoaded', function() {
        function enableResizing(divider) {
          const sidebar = divider.previousElementSibling;  // 分隔条左侧的 sidebar
          let isResizing = false;
    
          divider.addEventListener('mousedown', function(e) {
            isResizing = true;
            document.body.style.cursor = 'ew-resize';
            document.body.style.userSelect = 'none';
          });
    
          document.addEventListener('mousemove', function(e) {
            if (!isResizing) return;
            const newSidebarWidth = Math.max(200, Math.min(600, e.clientX)); // 限制宽度范围
            sidebar.style.flex = `0 0 ${newSidebarWidth}px`;
            
             // 调整所有表格列宽
            $('.dataTable').DataTable().columns.adjust();
          });
    
          document.addEventListener('mouseup', function() {
            if (isResizing) {
              isResizing = false;
              document.body.style.cursor = '';
              document.body.style.userSelect = '';
              
              // 再次确保表格布局正确
              $('.dataTable').DataTable().columns.adjust();
            }
          });
        }
    
        function bindResizableDividers() {
          document.querySelectorAll('.resizable-divider').forEach(function(divider) {
            if (!divider.dataset.bound) { // 避免重复绑定
              enableResizing(divider);
              divider.dataset.bound = true; // 标记为已绑定
            }
          });
        }
    
        bindResizableDividers();
    
        // 分页切换后重新绑定
        $(document).on('shown.bs.tab', function() {
          bindResizableDividers();
          $('.dataTable').DataTable().columns.adjust();
        });
      });
      
      // 成功音效
      function playSuccessSound() {
        var audio = new Audio('https://www.goldenbeanllc.com/sounds/success-8bit.mp3');
        audio.play();
      }
      
      // 错误音效
      function playErrorSound() {
        var audio = new Audio('https://www.goldenbeanllc.com/sounds/error-8bit.mp3');
        audio.play();
      }
      
      // 右键点击查询库存页面
      $(document).ready(function() {
        $('#filtered_inventory_table_query').on('contextmenu', 'tr', function(event) {
          event.preventDefault();
          var rowIdx = $(this).index();
          
          Shiny.setInputValue('selected_inventory_row', rowIdx + 1, {priority: 'event'});
    
          $('#context-menu').css({
            display: 'block',
            left: event.pageX + 'px',
            top: event.pageY + 'px'
          });
        });
    
        $(document).on('click', function(event) {
          if (!$(event.target).closest('#context-menu').length) {
            $('#context-menu').hide();
          }
        });
      });
      
      // 成交额placeholder
      $(document).ready(function() {
        $('#transaction_amount').attr('placeholder', '成交额（$）');
      });
    "))
        )
      ),
      
      # 动态渲染选项卡
      uiOutput("dynamic_tabs")
    )
  },
  tags_top = tags$div(
    tags$h3("欢迎使用 ERP 系统", style = "color: #007BFF;"),
    tags$p("请输入用户名和密码登录")
  )
)
