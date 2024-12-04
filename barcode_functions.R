# barcode_functions.R
library(ggplot2)

# Export barcode to PDF
export_barcode_pdf <- function(sku, quantity) {
  pdf_file <- tempfile(fileext = ".pdf")
  pdf(pdf_file, width = 4, height = 2)
  for (i in 1:quantity) {
    barcode_data <- data.frame(label = sku)
    
    # Create barcode and label
    barcode_plot <- ggplot(barcode_data, aes(x = 1, y = 1, label = label)) +
      geom_text(family = "BarcodeFont", size = 12) +
      theme_void() +
      theme(panel.grid = element_blank())
    
    print(barcode_plot)
  }
  dev.off()
  pdf_file
}
