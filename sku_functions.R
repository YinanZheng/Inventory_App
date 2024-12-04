# Function to generate SKU

library(digest)

# Generate unique random letters or random digits based on the item name
generate_unique_code <- function(item_name, output_type = "letters", num_characters = 2) {
  if (!is.null(item_name) && item_name != "") {
    # Generate a hash value for item_name
    hash_value <- digest(item_name, algo = "crc32")
    
    # Convert the hash value to an integer to set seed
    hash_seed <- strtoi(substr(hash_value, 1, 8), base = 16)
    if (is.na(hash_seed) || hash_seed < 0) {
      hash_seed <- abs(as.integer(charToRaw(item_name)) %% .Machine$integer.max)
    }
    
    # Set seed to ensure uniqueness based on item_name
    set.seed(hash_seed)
    
    # Generate random output based on the specified type
    if (output_type == "letters") {
      # Generate random letters
      random_output <- paste0(sample(LETTERS, num_characters, replace = TRUE), collapse = "")
    } else if (output_type == "digits") {
      # Generate random digits
      random_output <- paste0(sample(0:9, num_characters, replace = TRUE), collapse = "")
    } else {
      stop("Invalid output_type. Please use 'letters' or 'digits'.")
    }
    
    return(random_output)
  } else {
    return("")
  }
}

generate_sku <- function(item_type_data, major_type, minor_type, item_name, cost) {
  if (is.null(major_type) || is.null(minor_type) || is.null(item_name) || is.null(cost)) {
    return("")
  }
  
  # Format cost as three-digit value
  formatted_cost <- sprintf("%03.0f", round(cost))
  
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
  paste0(major_type_sku, formatted_cost, minor_type_sku, 
         generate_unique_code(item_name, "digits", 1),
         generate_unique_code(item_name, "letters", 1))
}
