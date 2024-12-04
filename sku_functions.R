# Function to generate SKU
generate_sku <- function(item_type_data, major_type, minor_type, item_name, cost) {
  if (is.null(major_type) || is.null(minor_type) || is.null(item_name) || is.null(cost)) {
    return("")
  }
  
  # Format cost as three-digit value
  formatted_cost <- sprintf("%03.0f", round(cost))
  
  # Generate two random letters based on the item name
  random_letters <- if (!is.null(item_name) && item_name != "") {
    set.seed(as.integer(Sys.time()))
    paste0(sample(LETTERS, 2, replace = TRUE), collapse = "")
  } else {
    ""
  }
  
  # Get major and minor type SKU values
  major_type_sku <- item_type_data %>%
    filter(MajorType == major_type) %>%
    pull(MajorTypeSKU) %>%
    unique()
  minor_type_sku <- item_type_data %>%
    filter(MinorType == minor_type) %>%
    pull(MinorTypeSKU) %>%
    unique()
  
  # Create the SKU
  paste0(major_type_sku, minor_type_sku, formatted_cost, random_letters)
}