SELECT schema_name, default_character_set_name FROM information_schema.schemata WHERE schema_name = 'inventory_system';
SHOW FULL COLUMNS FROM maker_list;
SHOW FULL COLUMNS FROM inventory;
SHOW FULL COLUMNS FROM item_type_data;

ALTER TABLE maker_list CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

DROP TABLE maker_list;
DROP TABLE item_type_data;



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
   
DELETE FROM maker_list
WHERE Name = '';



sudo mv /tmp/item_type_list.csv /var/lib/mysql-files/
sudo chmod 644 /var/lib/mysql-files/item_type_list.csv

LOAD DATA INFILE '/var/lib/mysql-files/item_type_list.csv'
INTO TABLE item_type_data
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(MajorType, MajorTypeSKU, MinorType, MinorTypeSKU);

SELECT * FROM item_type_data;
 