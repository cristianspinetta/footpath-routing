CREATE USER 'easy-travel-user'@'%' IDENTIFIED BY 'easyTravel';

CREATE DATABASE IF NOT EXISTS `easy-travel`;

GRANT ALL PRIVILEGES ON `easy-travel`.* TO 'easy-travel-user'@'%';