handle_inventory_submission <- function(added_items_df) {
  tryCatch({
    
    if (nrow(added_items_df) == 0) {
      showNotification("请先添加至少一个商品再确认!", type = "error")
      return()
    }
    
    # Retrieve total package shipping cost from the UI
    total_shipping_cost <- input$new_shipping_cost
    if (is.null(total_shipping_cost) || total_shipping_cost <= 0) {
      total_shipping_cost <- 0  # Default to 0 if invalid
    }
    
    unit_shipping_cost <- total_shipping_cost / sum(added_items_df$Quantity)
    
    for (i in 1:nrow(added_items_df)) {
      sku <- added_items_df$SKU[i]
      maker <- added_items_df$Maker[i]
      major_type <- added_items_df$MajorType[i]
      minor_type <- added_items_df$MinorType[i]
      item_name <- added_items_df$ItemName[i]
      quantity <- added_items_df$Quantity[i]
      product_cost <- added_items_df$ProductCost[i]
      new_image_path <- added_items_df$ItemImagePath[i]
      
      # 检查 SKU 是否已存在
      existing_item <- dbGetQuery(con, "SELECT * FROM inventory WHERE SKU = ?", params = list(sku))
      
      # 如果 SKU 已存在，检查图片路径
      if (nrow(existing_item) > 0) {
        new_quantity <- existing_item$Quantity + quantity
        new_ave_product_cost <- ((existing_item$ProductCost * existing_item$Quantity) + (product_cost * quantity)) / new_quantity
        new_ave_shipping_cost <- ((existing_item$ShippingCost * existing_item$Quantity) + (unit_shipping_cost * quantity)) / new_quantity
        
        # 如果有新图片上传，为图片生成唯一路径
        if (!is.na(new_image_path) && new_image_path != "") {
          unique_image_name <- paste0(sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
          final_image_path <- file.path("/var/www/images", unique_image_name)
          
          tryCatch({
            # 保存新图片
            file.rename(new_image_path, final_image_path)
            new_image_path <- final_image_path
          }, error = function(e) {
            showNotification(paste("图片保存失败! SKU:", sku), type = "error")
            new_image_path <- existing_item$ItemImagePath  # 回退为原始路径
          })
        } else {
          # 未上传新图片，保留现有图片路径
          new_image_path <- existing_item$ItemImagePath
        }
        
        # 更新库存数据
        dbExecute(con, "UPDATE inventory 
                      SET Quantity = ?, ProductCost = ?, ShippingCost = ?, ItemImagePath = ?, updated_at = NOW() 
                      WHERE SKU = ?",
                  params = list(new_quantity, round(new_ave_product_cost, 2), round(new_ave_shipping_cost, 2), new_image_path, sku))
        
        showNotification(paste("库存更新成功! SKU:", sku, ", 当前库存数:", new_quantity), type = "message")
      } else {
        # 如果 SKU 不存在，插入新商品
        unique_image_name <- if (!is.na(new_image_path) && new_image_path != "") {
          paste0(sku, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".jpg")
        } else {
          NA
        }
        
        if (!is.na(unique_image_name)) {
          final_image_path <- file.path("/var/www/images", unique_image_name)
          
          tryCatch({
            # 保存新图片
            file.rename(new_image_path, final_image_path)
            new_image_path <- final_image_path
          }, error = function(e) {
            showNotification(paste("新商品图片保存失败! SKU:", sku), type = "error")
          })
        }
        
        dbExecute(con, "INSERT INTO inventory 
                      (SKU, Maker, MajorType, MinorType, ItemName, Quantity, ProductCost, ShippingCost, ItemImagePath, created_at, updated_at) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())",
                  params = list(sku, maker, major_type, minor_type, item_name, quantity, 
                                round(product_cost, 2), round(unit_shipping_cost, 2), new_image_path))
        
        showNotification(paste("新商品添加成功! SKU:", sku, ", 商品名:", item_name), type = "message")
      }
    }
    
    # 刷新库存数据
    inventory({dbGetQuery(con, "SELECT * FROM inventory")})
    
    showNotification("库存已成功更新！", type = "message")
    
    
    ### 同时添加信息到 unique_items 表中
    # Prepare data for batch insertion
    batch_data <- do.call(rbind, lapply(1:nrow(added_items_df), function(i) {
      sku <- added_items_df$SKU[i]
      quantity <- added_items_df$Quantity[i]
      product_cost <- added_items_df$ProductCost[i]
      
      if (quantity <= 0 || is.na(product_cost) || product_cost <= 0) {
        return(NULL)  # Skip invalid rows
      }
      
      # Create rows for each quantity
      t(replicate(quantity, c(
        UUIDgenerate(),
        as.character(sku),
        as.numeric(product_cost),
        as.numeric(unit_shipping_cost),
        "国内入库",
        "无瑕",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )))
    }))
    
    # Validate data
    if (is.null(batch_data) || nrow(batch_data) == 0) {
      showNotification("批量数据无效，请检查输入！", type = "error")
      return()
    }
    
    # Convert to data frame
    batch_data <- as.data.frame(batch_data, stringsAsFactors = FALSE)
    # colnames(batch_data) <- c("UniqueID", "SKU", "ProductCost", "DomesticShippingCost", "Status", "Defect", "DomesticEntryTime")
    
    # Insert into database
    dbBegin(con)
    tryCatch({
      # Insert rows one by one
      for (i in 1:nrow(batch_data)) {
        # Ensure the parameters are passed as an unnamed vector
        dbExecute(con, "
      INSERT INTO unique_items (UniqueID, SKU, ProductCost, DomesticShippingCost, Status, Defect, DomesticEntryTime) 
      VALUES (?, ?, ?, ?, ?, ?, ?)",
                  unname(as.vector(batch_data[i, ]))  # Use `unname` to avoid named parameters
        )
      }
      dbCommit(con)  # Commit transaction
      showNotification("所有物品已成功入库到国内仓！", type = "message")
      # 切换触发器值，确保刷新
      current_value <- refresh_trigger()
      refresh_trigger(!current_value) 
    }, error = function(e) {
      dbRollback(con)  # Rollback on error
      showNotification(paste("批量入库失败:", e$message), type = "error")
    })
    
    # Clear added items and reset input fields
    added_items(create_empty_inventory())
    
    # 重置文件输入框
    shinyjs::reset("new_item_image")
    uploaded_file(NULL)
  }, error = function(e) {
    # Catch and display errors
    showNotification(paste("发生错误:", e$message), type = "error")
  })
}
