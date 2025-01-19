library(DBI)

db_connection <- function() {
  dbConnect(
    RMariaDB::MariaDB(),
    dbname = "inventory_system",
    host = "localhost",
    user = "root",
    password = "goldenbeanllc",
    encoding = "utf8mb4"
  )
}

archive_orders_and_items <- function() {
  con <- db_connection()
  
  tryCatch({
    # 记录开始时间
    start_time <- Sys.time()
    message(sprintf("[%s] 开始归档任务...", format(start_time, "%Y-%m-%d %H:%M:%S")))
    
    # 开启事务
    dbBegin(con)
    message(sprintf("[%s] 数据库事务开始", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    
    # 查询归档订单信息
    orders_to_archive <- dbGetQuery(con, "
      SELECT OrderID
      FROM orders
      WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY);
    ")
    order_count <- nrow(orders_to_archive)
    
    # 查询归档物品信息
    items_to_archive <- dbGetQuery(con, "
      SELECT UniqueID
      FROM unique_items
      WHERE OrderID IN (
        SELECT OrderID
        FROM orders
        WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY)
      );
    ")
    item_count <- nrow(items_to_archive)
    
    # 输出查询结果
    message(sprintf("[%s] 查询完成：%d 条订单，%d 条物品需要归档", 
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), order_count, item_count))
    
    # 归档订单到 orders_archive
    dbExecute(con, "
      INSERT INTO orders_archive
      SELECT *
      FROM orders
      WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY);
    ")
    message(sprintf("[%s] 已归档 %d 条订单到 orders_archive", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), order_count))
    
    # 归档物品到 unique_items_archive
    dbExecute(con, "
      INSERT INTO unique_items_archive
      SELECT *
      FROM unique_items
      WHERE OrderID IN (
        SELECT OrderID
        FROM orders
        WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY)
      );
    ")
    message(sprintf("[%s] 已归档 %d 条物品到 unique_items_archive", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), item_count))
    
    # 删除 unique_items 中已归档的物品
    dbExecute(con, "
      DELETE FROM unique_items
      WHERE OrderID IN (
        SELECT OrderID
        FROM orders
        WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY)
      );
    ")
    message(sprintf("[%s] 已从 unique_items 表中删除 %d 条物品", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), item_count))
    
    # 删除 orders 中已归档的订单
    dbExecute(con, "
      DELETE FROM orders
      WHERE OrderStatus = '送达' AND updated_at < DATE_SUB(NOW(), INTERVAL 90 DAY);
    ")
    message(sprintf("[%s] 已从 orders 表中删除 %d 条订单", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), order_count))
    
    # 提交事务
    dbCommit(con)
    end_time <- Sys.time()
    message(sprintf("[%s] 归档任务完成，总耗时 %.2f 秒", format(end_time, "%Y-%m-%d %H:%M:%S"), as.numeric(difftime(end_time, start_time, units = "secs"))))
    
  }, error = function(e) {
    # 回滚事务
    dbRollback(con)
    message(sprintf("[%s] 归档任务失败：%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), e$message))
  })
}
