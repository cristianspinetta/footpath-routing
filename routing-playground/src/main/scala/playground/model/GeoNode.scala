package playground.model

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.sql.PreparedStatement

import mapdomain.graph.{BaseEntity, Coordinate}
import scalikejdbc.{DBSession, WrappedResultSet, _}

import scala.math._


case class GeoNode(point: Coordinate, override val id: Option[Long] = None) extends BaseEntity

object GeoNode extends SQLSyntaxSupport[GeoNode] {
  override val tableName = "geo_3_node"

  def createTable = DB autoCommit { implicit s ⇒
    sql"""
              CREATE TABLE IF NOT EXISTS `geo_node` (
                `id` bigint(20) NOT NULL AUTO_INCREMENT,
                `point` Point,
                `lat` DOUBLE,
                `lng` DOUBLE,
                PRIMARY KEY (`id`)
               );
   """.execute.apply()
  }

  def createTable2 = DB autoCommit { implicit s ⇒
    sql"""
              CREATE TABLE IF NOT EXISTS `geo_2_node` (
                `id` bigint(20) NOT NULL AUTO_INCREMENT,
                `point` Point,
                `lat` DOUBLE,
                `lng` DOUBLE,
                PRIMARY KEY (`id`)
               );
   """.execute.apply()
  }

  def createTable3 = DB autoCommit { implicit s ⇒
    sql"""
              CREATE TABLE IF NOT EXISTS `geo_3_node` (
                `id` bigint(20) NOT NULL AUTO_INCREMENT,
                `point` Point NOT NULL,
                `lat` DOUBLE,
                `lng` DOUBLE,
                PRIMARY KEY (`id`)
               ) ENGINE=Aria DEFAULT CHARSET=utf8;
   """.execute.apply()
  }
}

case class SQLPoint(geoNode: GeoNode) {
  lazy val sql: String = s"POINT(${geoNode.point.radLongitude} ${geoNode.point.radLatitude})"
}

object GeoNodeRepository {

  val n = GeoNode.syntax("n")

  def geoNode(c: SyntaxProvider[GeoNode])(rs: WrappedResultSet): GeoNode = geoNode(rs)

  private def geoNode(implicit rs: WrappedResultSet): GeoNode = {
    GeoNode(Coordinate(rs.double("lat"), rs.double("lng")))
  }

  def create(geoNode: GeoNode)(implicit session: DBSession = GeoNode.autoSession): GeoNode = {

    val sqlPoint = SQLPoint(geoNode)

    val id = sql"""insert into geo_3_node (point, lat, lng) values (PointFromText(${sqlPoint.sql}), ${geoNode.point.radLatitude}, ${geoNode.point.radLongitude})"""
      //    val id = withSQL {
      //      insert.into(GeoNode)
      //        .append(sqls"""(point, lat, lng) values (PointFromText(${sqlPoint.sql}), ${geoNode.point.latitude}, ${geoNode.point.longitude})""")
      //      insert.into(GeoNode).namedValues(
      //        GeoNode.column.point -> sqls"PointFromText(${sqlPoint.sql})",
      //        sqls"lat" -> geoNode.point.latitude,
      //        sqls"lng" -> geoNode.point.longitude
      //      )

      //    }
      .updateAndReturnGeneratedKey().apply()

    geoNode.copy(id = Some(id))
  }

  // distance must be in Km
  def findNearestWithRangeScanOnly(coordinate: Coordinate, distance: Double)(implicit session: DBSession = GeoNode.autoSession): List[GeoNode] = {
    // angular distance in radians on a great circle
    val radDistance = distance / Coordinate.radius
    withSQL {
      select(sqls"x(point) lng, y(point) lat").from(GeoNode as n)
        .where.append(sqls"ST_Distance(point(${coordinate.radLongitude},${coordinate.radLatitude}), point) <= $radDistance")
    }.map(geoNode(n)).list().apply()
  }

  // distance must be in Km
  def findNearestWithRangeScanAndBounding(coordinate: Coordinate, distance: Double)(implicit session: DBSession = GeoNode.autoSession): List[GeoNode] = {
    // angular distance in radians on a great circle
    val radDistance = distance / Coordinate.radius

    val bound: BoundedGeoLocation = BoundedGeoLocation.boundByDistance(coordinate, distance)

    val sql = if (bound.meridian180WithinDistance)
      sql"""select x(point) lng, y(point) lat from geo_3_node
            |where
            | (y(point) >= ${bound.min.radLatitude} and y(point) <= ${bound.max.radLatitude})
            | and (x(point) >= ${bound.min.radLongitude} or x(point) <= ${bound.max.radLongitude})
            | and ST_Distance(point(${coordinate.radLongitude},${coordinate.radLatitude}), point) <= $radDistance""".stripMargin
    else
      sql"""select x(point) lng, y(point) lat from geo_3_node
            |where
            | (y(point) >= ${bound.min.radLatitude} and y(point) <= ${bound.max.radLatitude})
            | and (x(point) >= ${bound.min.radLongitude} and x(point) <= ${bound.max.radLongitude})
            | and ST_Distance(point(${coordinate.radLongitude},${coordinate.radLatitude}), point) <= $radDistance""".stripMargin

//    sql"""select x(point) lng, y(point) lat from geo_node
//          |where
//          | y(point) >= ${bound.min.radLatitude} and y(point) <= ${bound.max.radLatitude}
//          | and x(point) >= ${bound.min.radLongitude} and x(point) <= ${bound.max.radLongitude}
//          | and ST_Distance(point(${coordinate.radLongitude},${coordinate.radLongitude}), point) <= $radDistance""".stripMargin
      sql.map(geoNode(n)).list().apply()
  }

  def findNearestByWhere(coordinate: Coordinate, where: SQLSyntax)(implicit session: DBSession = GeoNode.autoSession): List[GeoNode] = {
    withSQL {
      select(sqls"lat, lng").from(GeoNode as n)
        .where.append(where)
    }.map(geoNode(n)).list().apply()
  }
}

case class BoundedGeoLocation(min: Coordinate, max: Coordinate) {
  val meridian180WithinDistance = min.radLongitude > max.radLongitude
}

object BoundedGeoLocation {

  def boundByDistance(coordinate: Coordinate, distance: Double, radius: Double = Coordinate.radius) = {

    if (radius < 0d || distance < 0d)
      throw new IllegalArgumentException()

    // angular distance in radians on a great circle
    val radDist = distance / radius

    val minLatCandidate = coordinate.radLatitude - radDist
    val maxLatCandidate = coordinate.radLatitude + radDist

    val MIN_LAT = (-90d).toRadians  // -PI/2
    val MAX_LAT = 90d.toRadians   //  PI/2
    val MIN_LON = (-180d).toRadians // -PI
    val MAX_LON = 180d.toRadians  //  PI

    val (maxLat, minLat, minLng, maxLng) = if (minLatCandidate > MIN_LAT && maxLatCandidate < MAX_LAT) {
      val deltaLon = asin(sin(radDist) / cos(coordinate.radLatitude))

      val minLngCandidate = coordinate.radLongitude - deltaLon
      val finalMinLng = if (minLngCandidate < MIN_LON) minLngCandidate + 2d * Pi else minLngCandidate

      val maxLngCandidate = coordinate.radLongitude + deltaLon
      val finalMaxLng = if (maxLngCandidate > MAX_LON) maxLngCandidate - 2d * Pi else maxLngCandidate

      (maxLatCandidate, minLatCandidate, finalMinLng, finalMaxLng)
    } else {
      // a pole is within the distance
      val finalMinLat = max(minLatCandidate, MIN_LAT)
      val finalMaxLat = min(maxLatCandidate, MAX_LAT)
      val finalMinLon = MIN_LON
      val finalMaxLon = MAX_LON
      (finalMaxLat, finalMinLat, finalMinLon, finalMaxLon)
    }
    BoundedGeoLocation(Coordinate(minLat.toDegrees, minLng.toDegrees), Coordinate(maxLat.toDegrees, maxLng.toDegrees))
  }

}
