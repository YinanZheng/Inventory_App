# 定义列名映射函数
map_column_names <- function(data, column_mapping) {
  existing_columns <- names(data)
  mapped_columns <- sapply(existing_columns, function(col) {
    column_mapping[[col]] %||% col  # 如果没有映射，保留原始列名
  })
  setNames(data, mapped_columns)  # 更新列名
}

generate_datatable <- function(data, column_mapping, selectable = TRUE) {
  data <- map_column_names(data, column_mapping)
  datatable(data, selection = if (selectable) 'single' else 'none', rownames = FALSE)
}