# barcode_functions.R
library(ggplot2)

# Export barcode to PDF
export_barcode_pdf <- function(sku, quantity, font_family) {
  # Create a temporary file path for the PDF
  pdf_file <- tempfile(fileext = ".pdf")
  
  # 设置页面宽度和高度（单位为英寸）
  pdf_width <- 1.181
  pdf_height <- 0.787
  
  # 计算宽高比（注意这里宽和高顺序）
  aspect_ratio <- pdf_height / pdf_width

  # Start PDF device with specified page size
  pdf(pdf_file, width = pdf_width, height = pdf_height)
  
  # Loop through quantity to create each page for the barcode
  for (i in 1:quantity) {
    barcode_data <- data.frame(label = sku)
    
    # Create barcode and label
    barcode_plot <- ggplot(barcode_data, aes(x = 0, y = 0, label = label)) +
      geom_text(family = font_family, size = 15, vjust = 0.5, hjust = 0.5) +
      theme_void() +
      theme(
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0)  # 去掉边距
      ) +
      coord_fixed(ratio = aspect_ratio, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) 
    
    # Print the plot to the PDF, one plot per page
    print(barcode_plot)
  }
  
  # Close the PDF device
  dev.off()
  
  # Return the PDF file path
  pdf_file
}
