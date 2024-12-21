SELECT schema_name, default_character_set_name FROM information_schema.schemata WHERE schema_name = 'inventory_system';

SHOW FULL COLUMNS FROM maker_list;
SHOW FULL COLUMNS FROM inventory;
SHOW FULL COLUMNS FROM item_type_data;


-- Populate maker_list using csv files
-- Use NotePad ++ to convert the encoding to UTF-8
sudo mv /tmp/maker_list.csv /var/lib/mysql-files/
sudo chmod 644 /var/lib/mysql-files/maker_list.csv


USE inventory_system;
LOAD DATA INFILE '/var/lib/mysql-files/maker_list.csv'
INTO TABLE maker_list
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(Name, Pinyin);

UPDATE maker_list
SET Name = TRIM(REPLACE(Name, '\r', '')),
    Pinyin = TRIM(REPLACE(Pinyin, '\r', ''));

SELECT * FROM maker_list;




-- Populate item_type_data using csv files
-- Use NotePad ++ to convert the encoding to UTF-8
sudo mv /tmp/item_type_list.csv /var/lib/mysql-files/
sudo chmod 644 /var/lib/mysql-files/item_type_list.csv

LOAD DATA INFILE '/var/lib/mysql-files/item_type_list.csv'
INTO TABLE item_type_data
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(MajorType, MajorTypeSKU, MinorType, MinorTypeSKU);

UPDATE item_type_data
SET MajorType = TRIM(REPLACE(MajorType, '\r', '')),
    MajorTypeSKU = TRIM(REPLACE(MajorTypeSKU, '\r', '')),
    MinorType = TRIM(REPLACE(MinorType, '\r', '')),
    MinorTypeSKU = TRIM(REPLACE(MinorTypeSKU, '\r', ''));

SELECT * FROM item_type_data;
 
--
USE inventory_system;
SELECT * FROM inventory;
SELECT * FROM unique_items;
SELECT * FROM maker_list;
SELECT * FROM item_type_data;

-- Remove all record
USE inventory_system;

DELETE FROM maker_list;
DELETE FROM item_type_data;
DELETE FROM unique_items;
DELETE FROM inventory;

-- Delete a record using Maker's Name
DELETE FROM maker_list
WHERE Name = '';

-- Delete a record using SKU
DELETE FROM inventory
WHERE SKU = 'GB-ZK-3TMM';

DELETE FROM item_type_data
WHERE id BETWEEN 17 AND 22;

-- Remove table completely
USE inventory_system;
DROP TABLE unique_items;
DROP TABLE inventory;
DROP TABLE maker_list;
DROP TABLE item_type_data;




