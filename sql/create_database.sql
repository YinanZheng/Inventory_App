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
  SKU VARCHAR(50) PRIMARY KEY,  -- SKU must be unique
  Maker VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Supplier/Maker name
  MajorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Major category
  MinorType VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Minor category
  ItemName VARCHAR(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL, -- Name of the item
  ProductCost DECIMAL(10, 2) NOT NULL DEFAULT 0.00, -- Average cost with 2 decimal places
  ShippingCost DECIMAL(10, 2) NOT NULL DEFAULT 0.00, -- Average shipping cost with 2 decimal places
  Quantity INT NOT NULL DEFAULT 0, -- Quantity in stock, default is 0
  ItemImagePath VARCHAR(255), -- Path or URL to item image
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, -- Automatically track creation time
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP -- Automatically track last update time
);

CREATE TABLE unique_items (
    UniqueID VARCHAR(36) PRIMARY KEY,             -- Unique identifier for each item
    SKU VARCHAR(50) NOT NULL,                     -- Foreign key referencing inventory table
    ProductCost DECIMAL(10, 2) NOT NULL,          -- unit product cost with 2 decimal places
    DomesticShippingCost DECIMAL(10, 2) NOT NULL, -- unit domestic shipping cost with 2 decimal places
    Status ENUM('采购', '国内入库', '国内出库', '国内售出', '美国入库', '美国售出', '退货') NOT NULL, -- status
    Defect ENUM('未知', '无瑕', '瑕疵', '修复') NOT NULL,
    PuchaseTime DATE,                         -- Timestamp for '采购'
    DomesticEntryTime DATE,                   -- Timestamp for '国内入库'
    DomesticExitTime DATE,                    -- Timestamp for '国内出库'
    DomesticSoldTime DATE,                    -- Timestamp for '国内售出'
    UsEntryTime DATE,                         -- Timestamp for '美国入库'
    UsSoldTime DATE,                          -- Timestamp for '美国售出'
    ReturnTime DATE,                          -- Timestamp for '退货'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, -- Creation timestamp
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, -- Last update timestamp
    FOREIGN KEY (SKU) REFERENCES inventory(SKU)  -- Relationship with inventory table
);
