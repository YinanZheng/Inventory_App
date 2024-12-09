SELECT schema_name, default_character_set_name FROM information_schema.schemata WHERE schema_name = 'inventory_system';
SHOW FULL COLUMNS FROM maker_list;
SHOW FULL COLUMNS FROM inventory;
SHOW FULL COLUMNS FROM item_type_data;

ALTER TABLE maker_list CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;


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