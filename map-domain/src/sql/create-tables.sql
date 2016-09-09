use `EASY_TRAVEL`;

CREATE TABLE IF NOT EXISTS `OsmVertex` (
                `id` bigint(20) NOT NULL,
                `coordinate` Point NOT NULL,
                PRIMARY KEY (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `OsmStreetEdge` (
                `id` bigint(20) NOT NULL AUTO_INCREMENT,
                `vertexStartId` bigint(20) NOT NULL,
                `vertexEndId` bigint(20) NOT NULL,
                `distance` DOUBLE NOT NULL,
                  `wayId` bigint(20) DEFAULT NULL,
                PRIMARY KEY (`id`),
                 FOREIGN KEY (`vertexStartId`) REFERENCES `OsmVertex` (`id`),
                FOREIGN KEY (`vertexEndId`) REFERENCES `OsmVertex` (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `Ramp` (
               	`id` varchar(255) NOT NULL,
               	`street` varchar(255) DEFAULT NULL,
               	`number` int DEFAULT NULL,
               	`address` varchar(255) DEFAULT NULL,
                `coordinate` Point NOT NULL,
                PRIMARY KEY (`id`),
                SPATIAL INDEX(coordinate)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `Path` (
               	`id` bigint(20) NOT NULL AUTO_INCREMENT,
               	`coordinates` varchar(255) DEFAULT NULL,
               	PRIMARY KEY (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `Stop` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`coordinate` Point NOT NULL,
       	`cellNumber` int DEFAULT NULL,
       	`nextStopId` bigint(20) DEFAULT NULL,
       	`previousStopId` bigint(20) DEFAULT NULL,
       	`pathId` bigint(20) DEFAULT NULL,
       	`travelInfoId` bigint(20) DEFAULT NULL,
       	`isAccessible` boolean DEFAULT true,
       	PRIMARY KEY (`id`),
       	FOREIGN KEY (`nextStopId`) REFERENCES `Stop` (`id`),
       	FOREIGN KEY (`previousStopId`) REFERENCES `Stop` (`id`),
       	FOREIGN KEY (`pathId`) REFERENCES `Path` (`id`),
        SPATIAL INDEX(coordinate)
       ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `TravelInfo` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`description` varchar(1000) DEFAULT NULL,
       	`firstStopId` bigint(20) DEFAULT NULL,
       	`lastStopId` bigint(20) DEFAULT NULL,
       	PRIMARY KEY (`id`),
       	FOREIGN KEY (`firstStopId`) REFERENCES `Stop` (`id`),
       	FOREIGN KEY (`lastStopId`) REFERENCES `Stop` (`id`)
       ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `SidewalkVertex` (
  `id` bigint(20) NOT NULL,
  `coordinate` Point NOT NULL,
  `streetVertexBelongToId` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`streetVertexBelongToId`) REFERENCES `OsmVertex` (`id`)
) ENGINE=Aria DEFAULT CHARSET=utf8;

ALTER TABLE `Stop` ADD CONSTRAINT fk_travelInfo_id FOREIGN KEY (travelInfoId) REFERENCES `TravelInfo` (`id`);

CREATE TABLE IF NOT EXISTS `StreetCrossingEdge` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `vertexStartId` bigint(20) NOT NULL,
  `vertexEndId` bigint(20) NOT NULL,
  `keyValue` VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`vertexStartId`) REFERENCES `SidewalkVertex` (`id`),
  FOREIGN KEY (`vertexEndId`) REFERENCES `SidewalkVertex` (`id`)
) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `SidewalkEdge` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `vertexStartId` bigint(20) NOT NULL,
  `vertexEndId` bigint(20) NOT NULL,
  `keyValue` VARCHAR(255) DEFAULT NULL,
  `streetEdgeBelongToId` bigint(20) NOT NULL,
  `side` int NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`vertexStartId`) REFERENCES `SidewalkVertex` (`id`),
  FOREIGN KEY (`vertexEndId`) REFERENCES `SidewalkVertex` (`id`),
  FOREIGN KEY (`streetEdgeBelongToId`) REFERENCES `OsmStreetEdge` (`id`)
) ENGINE=Aria DEFAULT CHARSET=utf8;