package mapdomain.repository

import scalikejdbc._

class DBInitializer {

  def start() = DB autoCommit { implicit s ⇒
    sql"""
DROP ALL OBJECTS;

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

CREATE TABLE IF NOT EXISTS `Ramp` (
               	`id` varchar(255) NOT NULL,
               	`street` varchar(255) DEFAULT NULL,
               	`number` int DEFAULT NULL,
               	`address` varchar(255) DEFAULT NULL,
                 `coordinateId` bigint(20) DEFAULT NULL,
                PRIMARY KEY (`id`),
                 FOREIGN KEY (`coordinateId`) REFERENCES `Coordinate` (`id`)
               );

CREATE TABLE IF NOT EXISTS `Path` (
               	`id` bigint(20) NOT NULL AUTO_INCREMENT,
               	`coordinates` varchar(255) DEFAULT NULL,
               	PRIMARY KEY (`id`)
               );

CREATE TABLE IF NOT EXISTS `Stop` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`coordinateId` bigint(20) DEFAULT NULL,
       	`cellNumber` int DEFAULT NULL,
       	`nextStopId` bigint(20) DEFAULT NULL,
       	`previousStopId` bigint(20) DEFAULT NULL,
       	`pathId` bigint(20) DEFAULT NULL,
       	`travelInfoId` bigint(20) DEFAULT NULL,
       	`isAccessible` boolean DEFAULT true,
       	PRIMARY KEY (`id`),
       	FOREIGN KEY (`coordinateId`) REFERENCES `Coordinate` (`id`),
       	FOREIGN KEY (`nextStopId`) REFERENCES `Stop` (`id`),
       	FOREIGN KEY (`previousStopId`) REFERENCES `Stop` (`id`),
       	FOREIGN KEY (`pathId`) REFERENCES `Path` (`id`)
       );

CREATE TABLE IF NOT EXISTS `TravelInfo` (
       	`id` bigint(20) NOT NULL AUTO_INCREMENT,
       	`description` varchar(1000) DEFAULT NULL,
       	`firstStopId` bigint(20) DEFAULT NULL,
       	`lastStopId` bigint(20) DEFAULT NULL,
       	PRIMARY KEY (`id`),
       	FOREIGN KEY (`firstStopId`) REFERENCES `Stop` (`id`),
       	FOREIGN KEY (`lastStopId`) REFERENCES `Stop` (`id`)
       );

ALTER TABLE `Stop` ADD CONSTRAINT fk_travelInfo_id FOREIGN KEY (travelInfoId) REFERENCES `TravelInfo` (`id`);

   """.execute.apply()
  }

}