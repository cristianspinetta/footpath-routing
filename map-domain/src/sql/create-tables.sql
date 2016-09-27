use `EASY_TRAVEL`; -- Or other DB

CREATE TABLE IF NOT EXISTS `street_vertex` (
                `id` bigint(20) NOT NULL,
                `coordinate` Point NOT NULL,
                PRIMARY KEY (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `street_edge` (
                `id` bigint(20) NOT NULL AUTO_INCREMENT,
                `vertexStartId` bigint(20) NOT NULL,
                `vertexEndId` bigint(20) NOT NULL,
                `distance` DOUBLE NOT NULL,
                  `wayId` bigint(20) DEFAULT NULL,
                PRIMARY KEY (`id`),
                 FOREIGN KEY (`vertexStartId`) REFERENCES `street_vertex` (`id`),
                FOREIGN KEY (`vertexEndId`) REFERENCES `street_vertex` (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `ramp` (
               	`id` varchar(255) NOT NULL,
               	`street` varchar(255) DEFAULT NULL,
               	`number` int DEFAULT NULL,
               	`address` varchar(255) DEFAULT NULL,
                `coordinate` Point NOT NULL,
                `isAccessible` boolean DEFAULT true,
                PRIMARY KEY (`id`),
                SPATIAL INDEX(coordinate)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `path` (
               	`id` bigint(20) NOT NULL AUTO_INCREMENT,
               	`coordinates` varchar(255) DEFAULT NULL,
               	PRIMARY KEY (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `stop` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`coordinate` Point NOT NULL,
       	`nextStopId` bigint(20) DEFAULT NULL,
       	`previousStopId` bigint(20) DEFAULT NULL,
       	`pathId` bigint(20) DEFAULT NULL,
       	`travelInfoId` bigint(20) DEFAULT NULL,
       	`isAccessible` boolean DEFAULT true,
       	PRIMARY KEY (`id`),
       	FOREIGN KEY (`nextStopId`) REFERENCES `stop` (`id`),
       	FOREIGN KEY (`previousStopId`) REFERENCES `stop` (`id`),
       	FOREIGN KEY (`pathId`) REFERENCES `path` (`id`),
        SPATIAL INDEX(coordinate)
       ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `travel_info` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`description` varchar(1000) DEFAULT NULL,
       	`firstStopId` bigint(20) DEFAULT NULL,
       	`lastStopId` bigint(20) DEFAULT NULL,
       	PRIMARY KEY (`id`),
       	FOREIGN KEY (`firstStopId`) REFERENCES `stop` (`id`),
       	FOREIGN KEY (`lastStopId`) REFERENCES `stop` (`id`)
       ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `sidewalk_vertex` (
  `id` bigint(20) NOT NULL,
  `coordinate` Point NOT NULL,
  `streetVertexBelongToId` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`streetVertexBelongToId`) REFERENCES `street_vertex` (`id`)
) ENGINE=Aria DEFAULT CHARSET=utf8;

ALTER TABLE `stop` ADD CONSTRAINT fk_travelInfo_id FOREIGN KEY (travelInfoId) REFERENCES `travel_info` (`id`);

CREATE TABLE IF NOT EXISTS `street_crossing_edge` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `vertexStartId` bigint(20) NOT NULL,
  `vertexEndId` bigint(20) NOT NULL,
  `keyValue` VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`vertexStartId`) REFERENCES `sidewalk_vertex` (`id`),
  FOREIGN KEY (`vertexEndId`) REFERENCES `sidewalk_vertex` (`id`)
) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `sidewalk_edge` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `vertexStartId` bigint(20) NOT NULL,
  `vertexEndId` bigint(20) NOT NULL,
  `keyValue` VARCHAR(255) DEFAULT NULL,
  `streetEdgeBelongToId` bigint(20) NOT NULL,
  `side` int NOT NULL,
  `isAccessible` boolean DEFAULT true,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`vertexStartId`) REFERENCES `sidewalk_vertex` (`id`),
  FOREIGN KEY (`vertexEndId`) REFERENCES `sidewalk_vertex` (`id`),
  FOREIGN KEY (`streetEdgeBelongToId`) REFERENCES `street_edge` (`id`)
) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE SPATIAL INDEX street_vertex_coordinate_index ON `street_vertex` (`coordinate`);
CREATE SPATIAL INDEX sidewalk_vertex_coordinate_index ON `sidewalk_vertex` (`coordinate`);

ALTER TABLE `street_crossing_edge`
  ADD COLUMN `rampStartId` varchar(255) DEFAULT NULL,
  ADD COLUMN `rampEndId` varchar(255) DEFAULT NULL,
  ADD CONSTRAINT fk_rampStart_id FOREIGN KEY (rampStartId) REFERENCES `ramp` (`id`),
  ADD CONSTRAINT fk_rampEnd_id FOREIGN KEY (rampEndId) REFERENCES `ramp` (`id`);