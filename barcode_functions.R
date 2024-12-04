# barcode_functions.R
library(ggplot2)

# Generate barcode image
generate_barcode <- function(sku) {
  temp_file <- tempfile(fileext = ".png")
  barcode_data <- data.frame(label = sku)
  barcode_plot <- ggplot(barcode_data, aes(x = 1, y = 1, label = label)) +
    geom_text(family = "Code 128", size = 15) +
    theme_void() +
    theme(panel.grid = element_blank())
  ggsave(temp_file, plot = barcode_plot, width = 4, height = 2, dpi = 300)
  base64enc::dataURI(file = temp_file, mime = "image/png")
}

# Export barcode to PDF
export_barcode_pdf <- function(sku, quantity) {
  pdf_file <- tempfile(fileext = ".pdf")
  pdf(pdf_file, width = 4, height = 2)
  for (i in 1:quantity) {
    barcode_data <- data.frame(label = sku)
    barcode_plot <- ggplot(barcode_data, aes(x = 1, y = 1, label = label)) +
      geom_text(family = "Code 128", size = 15) +
      theme_void() +
      theme(panel.grid = element_blank())
    print(barcode_plot)
  }
  dev.off()
  pdf_file
}