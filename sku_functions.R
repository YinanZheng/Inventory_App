# Function to generate SKU

library(digest)

# Generate unique random letters or random digits based on the item name
generate_unique_code <- function(item_name, output_type = "letters", num_characters = 2) {
  if (!is.null(item_name) && item_name != "") {
    # Generate a hash value for item_name
    hash_value <- digest(item_name, algo = "sha256")
    
    # Split the hash value into parts and convert to a long integer seed
    hash_parts <- strsplit(hash_value, "")[[1]]
    
    # Define the length of segments to process at a time
    segment_length <- 8
    total_sum <- 0
    
    # Iterate over the hash in segments to create a combined seed value
    for (i in seq(1, length(hash_parts), by = segment_length)) {
      segment <- paste0(hash_parts[i:min(i + segment_length - 1, length(hash_parts))], collapse = "")
      segment_value <- strtoi(segment, base = 16)
      
      if (!is.na(segment_value)) {
        total_sum <- (total_sum + segment_value) %% .Machine$integer.max
      }
    }
    
    # Ensure the seed is a positive integer
    hash_seed <- abs(total_sum)
    if (hash_seed == 0) {
      hash_seed <- 1  # Ensure the seed is at least 1
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
         generate_unique_code(item_name, "letters", 2))
}
