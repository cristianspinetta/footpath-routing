use `EASY_TRAVEL`; -- Or other DB

START TRANSACTION;

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
                  `streetInfoId` bigint(20) NOT NULL,
                PRIMARY KEY (`id`),
                 FOREIGN KEY (`vertexStartId`) REFERENCES `street_vertex` (`id`),
                FOREIGN KEY (`vertexEndId`) REFERENCES `street_vertex` (`id`),
                FOREIGN KEY (`streetInfoId`) REFERENCES `street_info` (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `street_info` (
                `id` bigint(20) NOT NULL AUTO_INCREMENT,
                `address` varchar(255) DEFAULT NULL,
                `wayId` bigint(20) DEFAULT NULL,
                PRIMARY KEY (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `ramp` (
               	`id` bigint(20) NOT NULL AUTO_INCREMENT,
               	`address` varchar(255) DEFAULT NULL,
                `coordinate` Point NOT NULL,
                `isAccessible` boolean DEFAULT true,
                `version` bigint(20) DEFAULT NULL,
                PRIMARY KEY (`id`),
                SPATIAL INDEX(coordinate)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `path` (
               	`id` bigint(20) NOT NULL AUTO_INCREMENT,
               	`coordinates` varchar(10000) DEFAULT NULL,
               	PRIMARY KEY (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `stop` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`coordinate` Point NOT NULL,
       	`nextStopId` bigint(20) DEFAULT NULL,
       	`previousStopId` bigint(20) DEFAULT NULL,
       	`sequence` bigint(20) NOT NULL,
       	`pathId` bigint(20) DEFAULT NULL,
       	`travelInfoId` bigint(20) NOT NULL,
       	`isAccessible` boolean DEFAULT true,
       	PRIMARY KEY (`id`)
       ) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `travel_info` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`description` varchar(1000) DEFAULT NULL,
       	`firstStopId` bigint(20) DEFAULT NULL,
       	`lastStopId` bigint(20) DEFAULT NULL,
        `branch` varchar(30) DEFAULT NULL,
        `name` varchar(30) DEFAULT NULL,
        `sentido` varchar(30) DEFAULT NULL,
        `type` varchar(30) DEFAULT NULL,
       	PRIMARY KEY (`id`)
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
  `rampStartId` bigint(20) DEFAULT NULL,
  `rampEndId` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`vertexStartId`) REFERENCES `sidewalk_vertex` (`id`),
  FOREIGN KEY (`vertexEndId`) REFERENCES `sidewalk_vertex` (`id`),
  FOREIGN KEY (`rampStartId`) REFERENCES `ramp` (`id`),
  FOREIGN KEY (`rampEndId`) REFERENCES `ramp` (`id`)
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

CREATE TABLE IF NOT EXISTS `public_transport_combination` (
  `fromStopId` bigint(20) NOT NULL,
  `fromCoordinate` Point NOT NULL,
  `toStopId` bigint(20) NOT NULL,
  `toCoordinate` Point NOT NULL,
  `fromTravelInfoId` bigint(20) NOT NULL,
  `toTravelInfoId` bigint(20) NOT NULL,
  `distance` float NOT NULL,
  `walkPath` varchar(20000) DEFAULT NULL,
  `enabled` boolean NOT NULL DEFAULT true,
  `cost` bigint(20) NOT NULL DEFAULT 999999999999999999,
  PRIMARY KEY (`fromStopId`, `toTravelInfoId`)
) ENGINE=Aria DEFAULT CHARSET=utf8;

CREATE SPATIAL INDEX `combination_from_coordinate_spatial_index` ON `public_transport_combination` (`fromCoordinate`);
CREATE SPATIAL INDEX `combination_to_coordinate_spatial_index` ON `public_transport_combination` (`toCoordinate`);


ALTER TABLE `stop` ADD CONSTRAINT `fk_stop_travel_info` FOREIGN KEY (`travelInfoId`) REFERENCES `travel_info`(`id`);
ALTER TABLE `stop` ADD CONSTRAINT `fk_stop_next_stop` FOREIGN KEY (`nextStopId`) REFERENCES `stop`(`id`);
ALTER TABLE `stop` ADD CONSTRAINT `fk_stop_previous_stop` FOREIGN KEY (`previousStopId`) REFERENCES `stop`(`id`);
ALTER TABLE `stop` ADD CONSTRAINT `fk_stop_path` FOREIGN KEY (`pathId`) REFERENCES `path`(`id`);
CREATE SPATIAL INDEX `stop_coordinate_spatial_index` ON `stop` (`coordinate`);

ALTER TABLE `travel_info` ADD CONSTRAINT `fk_travel_info_first_stop` FOREIGN KEY (`firstStopId`) REFERENCES `stop`(`id`);
ALTER TABLE `travel_info` ADD CONSTRAINT `fk_travel_info_last_stop` FOREIGN KEY (`lastStopId`) REFERENCES `stop`(`id`);

CREATE SPATIAL INDEX `street_vertex_coordinate_index` ON `street_vertex` (`coordinate`);
CREATE SPATIAL INDEX `sidewalk_vertex_coordinate_index` ON `sidewalk_vertex` (`coordinate`);

CREATE INDEX `public_transport_combination_from_ti_index` ON `public_transport_combination` (`fromTravelInfoId`);
CREATE INDEX `public_transport_combination_to_ti_index` ON `public_transport_combination` (`toTravelInfoId`);

CREATE TABLE IF NOT EXISTS `public_transport_combination_path` (
  `fromStopId` bigint(20) NOT NULL,
  `toTravelInfoId` bigint(20) NOT NULL,
  `walkPath` varchar(20000) DEFAULT NULL,
  PRIMARY KEY (`fromStopId`, `toTravelInfoId`)
) ENGINE=Aria DEFAULT CHARSET=utf8;

ALTER TABLE `public_transport_combination_path` ADD CONSTRAINT `fk_ptc_path_ptc` FOREIGN KEY (`fromStopId`, `toTravelInfoId`) REFERENCES `public_transport_combination`(`fromStopId`, `toTravelInfoId`);

/* Drop public_transport_combination columns, only run if public_transport_combination was created before
ALTER TABLE `public_transport_combination_path` DROP COLUMN `cost`,
                                                DROP COLUMN `walkPath`;
*/

COMMIT;

/* UPDATE */
START TRANSACTION;

ALTER TABLE `public_transport_combination` Add column `fromCoordinate` Point AFTER `fromStopId`;
UPDATE `public_transport_combination` ptc
    INNER JOIN `stop` s ON s.id = ptc.fromStopId
  SET `fromCoordinate` = `coordinate`;
ALTER TABLE `public_transport_combination` CHANGE column `fromCoordinate` `fromCoordinate` Point NOT NULL;

ALTER TABLE `public_transport_combination` Add column `toCoordinate` Point AFTER `toStopId`;
UPDATE `public_transport_combination` ptc
    INNER JOIN `stop` s ON s.id = ptc.toStopId
  SET `toCoordinate` = `coordinate`;
ALTER TABLE `public_transport_combination` CHANGE column `toCoordinate` `toCoordinate` Point NOT NULL;

CREATE SPATIAL INDEX `combination_from_coordinate_spatial_index` ON `public_transport_combination` (`fromCoordinate`);
CREATE SPATIAL INDEX `combination_to_coordinate_spatial_index` ON `public_transport_combination` (`toCoordinate`);

COMMIT;
