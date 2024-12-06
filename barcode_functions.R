export_barcode_pdf <- function(sku, quantity) {
  # Create a temporary file path for the PDF
  pdf_file <- tempfile(fileext = ".pdf")
  
  # 设置页面宽度和高度（单位为英寸）
  pdf_width <- 1.181
  pdf_height <- 0.787  
  
  # Start PDF device with specified page size
  pdf(pdf_file, width = pdf_width, height = pdf_height)
  
  # Loop through quantity to create each page for the barcode
  for (i in 1:quantity) {
    barcode_data <- data.frame(label = sku)
    
    # Create barcode using code128 font with adjusted vertical stretch
    barcode_plot <- ggplot(barcode_data, aes(x = 0, y = 0, label = label)) +
      geom_text(family = "code128", size = 9, vjust = 0.25, hjust = 0.5) +  # 增大字体并调整垂直位置以拉伸外观
      theme_void() +
      theme(
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0, 0)  # 去掉边距
      )
    
    # Print the barcode plot to the PDF, one plot per page
    print(barcode_plot)
    
    # Create SKU label below the barcode using consolas font with increased letter spacing
    grid::grid.text(sku, y = unit(0.25, "npc"), gp = grid::gpar(fontfamily = "consolas", cex = 1, col = "black"))
  }
  
  # Close the PDF device
  dev.off()
  
  # Return the PDF file path
  pdf_file
}

export_barcode_pdf(sku, 5)
