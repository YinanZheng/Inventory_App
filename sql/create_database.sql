CREATE DATABASE inventory_system;
USE inventory_system;

CREATE TABLE maker_list (
  id INT AUTO_INCREMENT PRIMARY KEY,
  Name VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  Pinyin VARCHAR(255)
);

CREATE TABLE item_type_data (
  id INT AUTO_INCREMENT PRIMARY KEY,
  MajorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  MajorTypeSKU VARCHAR(50),
  MinorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  MinorTypeSKU VARCHAR(50)
);

CREATE TABLE inventory (
  id INT AUTO_INCREMENT PRIMARY KEY, -- Unique identifier for each item
  SKU VARCHAR(50) NOT NULL UNIQUE, -- SKU must be unique and not null
  Maker VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Supplier/Maker name
  MajorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Major category
  MinorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Minor category
  ItemName VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Name of the item
  Cost DECIMAL(10, 2) NOT NULL DEFAULT 0.00, -- Average cost with 2 decimal places
  Quantity INT NOT NULL DEFAULT 0, -- Quantity in stock, default is 0
  ItemImagePath VARCHAR(255), -- Path or URL to item image
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, -- Automatically track creation time
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP -- Automatically track last update time
);

CREATE TABLE unique_items (
    UniqueID VARCHAR(36) PRIMARY KEY,             -- Unique identifier for each item
    SKU VARCHAR(50) NOT NULL,                     -- Foreign key referencing inventory table
    Cost DECIMAL(10, 2) NOT NULL,                 -- Average cost with 2 decimal places
    Status ENUM('国内仓入库', '国内仓出库', '美国仓入库', '美国仓出库') NOT NULL DEFAULT '国内仓入库', -- Current status
    DomesticEntryTime DATETIME,                   -- Timestamp for '国内仓入库'
    DomesticExitTime DATETIME,                    -- Timestamp for '国内仓出库'
    UsEntryTime DATETIME,                         -- Timestamp for '美国仓入库'
    UsExitTime DATETIME,                          -- Timestamp for '美国仓出库'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, -- Creation timestamp
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, -- Last update timestamp
    FOREIGN KEY (SKU) REFERENCES inventory(SKU),  -- Relationship with inventory table
    INDEX idx_status (Status),                    -- Index for Status
    INDEX idx_sku (SKU),                          -- Index for SKU
    INDEX idx_domestic_entry_time (DomesticEntryTime) -- Index for DomesticEntryTime
);
