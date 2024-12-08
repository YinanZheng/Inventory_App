CREATE DATABASE inventory_system;
USE inventory_system;

CREATE TABLE maker_list (
  id INT AUTO_INCREMENT PRIMARY KEY,
  Name VARCHAR(255),
  Pinyin VARCHAR(255)
);

CREATE TABLE item_type_data (
  id INT AUTO_INCREMENT PRIMARY KEY,
  MajorType VARCHAR(255),
  MajorTypeSKU VARCHAR(50),
  MinorType VARCHAR(255),
  MinorTypeSKU VARCHAR(50)
);

CREATE TABLE inventory (
  id INT AUTO_INCREMENT PRIMARY KEY,
  SKU VARCHAR(50),
  MajorType VARCHAR(255),
  MinorType VARCHAR(255),
  ItemName VARCHAR(255),
  Quantity INT,
  ItemImagePath VARCHAR(255) -- 存储图片路径或 URL
);