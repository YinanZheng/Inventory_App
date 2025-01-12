CREATE DATABASE inventory_system;
USE inventory_system;

CREATE TABLE `maker_list` (
  `id` int NOT NULL AUTO_INCREMENT,
  `Name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `Pinyin` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=202 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `item_type_data` (
  `id` int NOT NULL AUTO_INCREMENT,
  `MajorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `MajorTypeSKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `MinorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `MinorTypeSKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=167 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `inventory` (
  `SKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `Maker` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `MajorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `MinorType` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ItemName` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ProductCost` decimal(10,2) NOT NULL DEFAULT '0.00',
  `ShippingCost` decimal(10,2) NOT NULL DEFAULT '0.00',
  `Quantity` int NOT NULL DEFAULT '0',
  `ItemImagePath` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`SKU`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `unique_items` (
  `UniqueID` varchar(36) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `SKU` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ProductCost` decimal(10,2) NOT NULL,
  `DomesticShippingCost` decimal(10,2) NOT NULL,
  `Status` enum('采购','国内入库','国内出库','国内售出','美国入库','美国售出','美国发货','美国调货','退货') COLLATE utf8mb4_unicode_ci NOT NULL,
  `Defect` enum('未知','无瑕','瑕疵','修复') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `DefectNotes` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `PurchaseTime` date DEFAULT NULL,
  `DomesticEntryTime` date DEFAULT NULL,
  `DomesticExitTime` date DEFAULT NULL,
  `DomesticSoldTime` date DEFAULT NULL,
  `UsEntryTime` date DEFAULT NULL,
  `UsShippingTime` date DEFAULT NULL,
  `UsRelocationTime` date DEFAULT NULL,
  `UsSoldTime` date DEFAULT NULL,
  `ReturnTime` date DEFAULT NULL,
  `IntlShippingMethod` enum('海运','空运') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `IntlTracking` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `IntlShippingCost` decimal(10,2) NOT NULL DEFAULT '0.00',
  `OrderID` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `PurchaseCheck` tinyint(1) NOT NULL DEFAULT '0',
  PRIMARY KEY (`UniqueID`),
  KEY `SKU` (`SKU`),
  KEY `fk_orders_orderid` (`OrderID`),
  KEY `fk_intl_tracking` (`IntlTracking`),
  CONSTRAINT `fk_intl_tracking` FOREIGN KEY (`IntlTracking`) REFERENCES `intl_shipments` (`TrackingNumber`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `fk_orderid` FOREIGN KEY (`OrderID`) REFERENCES `orders` (`OrderID`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `unique_items_ibfk_1` FOREIGN KEY (`SKU`) REFERENCES `inventory` (`SKU`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci

CREATE TABLE `orders` (
  `OrderID` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `UsTrackingNumber` varchar(50) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `CustomerName` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `CustomerNetName` varchar(50) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `Platform` enum('Etsy','Shopify','TikTok','其他') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `OrderImagePath` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `OrderNotes` text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  `OrderStatus` enum('备货','预定','调货','装箱','发出','在途','送达') COLLATE utf8mb4_unicode_ci NOT NULL,
  `LabelStatus` enum('无','已传','印出') COLLATE utf8mb4_unicode_ci DEFAULT '无',
  `created_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`OrderID`),
  KEY `idx_customer_name` (`CustomerName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci 

CREATE TABLE `intl_shipments` (
  `TrackingNumber` VARCHAR(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `ShippingMethod` ENUM('海运', '空运') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `TotalCost` DECIMAL(10,2) NOT NULL DEFAULT 0.00,
  `Status` ENUM('待分配', '运输中', '已完成', '取消') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '待分配',
  `CreatedAt` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  `UpdatedAt` TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`TrackingNumber`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

CREATE TABLE `item_status_history` (
  `UniqueID` varchar(36) COLLATE utf8mb4_unicode_ci NOT NULL,
  `previous_status` enum('采购','国内入库','国内出库','国内售出','美国入库','美国售出','美国调货','退货','美国发货') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `previous_status_timestamp` timestamp NULL DEFAULT NULL,
  `change_time` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`UniqueID`,`change_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci  

CREATE TABLE `transactions` (
  `TransactionID` int NOT NULL AUTO_INCREMENT,
  `AccountType` enum('工资卡','美元卡','买货卡','一般户卡') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL,
  `Amount` decimal(10,2) NOT NULL,
  `Balance` decimal(10,2) DEFAULT '0.00',
  `Remarks` text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci,
  `TransactionTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`TransactionID`)
) ENGINE=InnoDB AUTO_INCREMENT=27 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci


DELIMITER //

CREATE TRIGGER after_transaction_insert
AFTER INSERT ON `transactions`
FOR EACH ROW
BEGIN
  DECLARE last_balance DECIMAL(10,2);

  -- 获取同一账户的最新余额（除当前插入的记录）
  SELECT Balance INTO last_balance
  FROM transactions
  WHERE AccountType = NEW.AccountType
    AND TransactionID < NEW.TransactionID
  ORDER BY TransactionTime DESC
  LIMIT 1;

  -- 如果没有上一次的记录，余额从 0 开始
  IF last_balance IS NULL THEN
    SET last_balance = 0.00;
  END IF;

  -- 更新当前记录的余额
  UPDATE transactions
  SET Balance = last_balance + NEW.Amount
  WHERE TransactionID = NEW.TransactionID;
END;
//

DELIMITER ;