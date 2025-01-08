autocompleteInputServer <- function(id, get_suggestions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 定义命名空间
    
    observeEvent(input$name, {
      current_input <- trimws(input$name)
      if (current_input == "") {
        runjs(sprintf("$('#%s').text('');", ns("hint")))  # 清空提示
      } else {
        suggestions <- req(get_suggestions())[startsWith(get_suggestions(), current_input)]  # 匹配前缀
        if (length(suggestions) > 0) {
          hint <- substr(suggestions[1], nchar(current_input) + 1, nchar(suggestions[1]))
          # 动态显示提示文字
          runjs(sprintf("
            const inputElement = document.getElementById('%s');
            const inputValue = '%s';
            const span = document.createElement('span');
            span.style.visibility = 'hidden';
            span.style.position = 'absolute';
            span.style.whiteSpace = 'nowrap';
            span.style.fontSize = window.getComputedStyle(inputElement).fontSize;
            span.innerHTML = inputValue.replace(/ /g, '&nbsp;');
            document.body.appendChild(span);
            const inputWidth = span.offsetWidth;
            document.body.removeChild(span);
            const hintElement = document.getElementById('%s');
            hintElement.textContent = '%s';
            hintElement.style.left = `${inputWidth + 17}px`;
            hintElement.style.fontStyle = 'italic';
          ", ns("name"), current_input, ns("hint"), hint))
        } else {
          runjs(sprintf("$('#%s').text('');", ns("hint")))  # 无匹配时清空提示
        }
      }
    })
  })
}
