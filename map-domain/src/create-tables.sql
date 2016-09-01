use `easy-travel`;

CREATE TABLE IF NOT EXISTS `Coordinate` (
	`id` bigint(20) NOT NULL AUTO_INCREMENT,
	`latitude` double DEFAULT NULL,
	`longitude` double DEFAULT NULL,
	PRIMARY KEY (`id`)
);

CREATE TABLE IF NOT EXISTS `OsmVertex` (
	`id` bigint(20) NOT NULL,
	`coordinateId` bigint(20) DEFAULT NULL,
	PRIMARY KEY (`id`),
	FOREIGN KEY (`coordinateId`) REFERENCES `Coordinate` (`id`)
);

CREATE TABLE IF NOT EXISTS `OsmStreetEdge` (
	`id` bigint(20) NOT NULL AUTO_INCREMENT,
	`vertexStartId` bigint(20) DEFAULT NULL,
	`vertexEndId` bigint(20) DEFAULT NULL,
	`distance` DOUBLE DEFAULT NULL,
  	`wayId` bigint(20) DEFAULT NULL,
	PRIMARY KEY (`id`),
  FOREIGN KEY (`vertexStartId`) REFERENCES `OsmVertex` (`id`),
	FOREIGN KEY (`vertexEndId`) REFERENCES `OsmVertex` (`id`)
);

CREATE TABLE IF NOT EXISTS `easy-travel`.`Ramp` (
	`id` bigint(20) NOT NULL AUTO_INCREMENT,
	`street` varchar(255) DEFAULT NULL,
	`number` int DEFAULT NULL,
	`address` varchar(255) DEFAULT NULL,
  `coordinateId` bigint(20) DEFAULT NULL,
	PRIMARY KEY (`id`),
  FOREIGN KEY (`coordinateId`) REFERENCES `Coordinate` (`id`)
);