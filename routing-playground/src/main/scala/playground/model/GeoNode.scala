package playground.model

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.sql.PreparedStatement

import mapdomain.graph.{BaseEntity, BoundedGeoLocation, Coordinate}
import scalikejdbc.{DBSession, WrappedResultSet, _}
import sql.SpatialSQLSupport

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

object GeoNodeRepository extends SpatialSQLSupport {

  val n = GeoNode.syntax("n")

  def geoNode(c: SyntaxProvider[GeoNode])(rs: WrappedResultSet): GeoNode = geoNode(rs)

  private def geoNode(implicit rs: WrappedResultSet): GeoNode = {
    GeoNode(Coordinate(rs.double("lat"), rs.double("lng")))
  }

  def create(geoNode: GeoNode)(implicit session: DBSession = GeoNode.autoSession): GeoNode = {

    val sqlPoint = SQLPoint(geoNode)

    val point = SQLSyntax.createUnsafely(s"PointFromText('POINT(${sqlPoint.geoNode.point.latitude} ${sqlPoint.geoNode.point.longitude})')")

    val q = sql"""insert into geo_3_node (point, lat, lng) values ($point, ${sqlPoint.geoNode.point.latitude}, ${sqlPoint.geoNode.point.longitude})"""

//    val id = q.updateAndReturnGeneratedKey().apply()

    val id = withSQL {

      insert.into(GeoNode).namedValues(
        GeoNode.column.point -> positionToSQL(Coordinate(geoNode.point.latitude, geoNode.point.longitude)))
    }

//    val id = sql"""insert into geo_3_node (point, lat, lng) values (PointFromText(${sqlPoint.sql}), ${geoNode.point.radLatitude}, ${geoNode.point.radLongitude})"""
//      //    val id = withSQL {
//      //      insert.into(GeoNode)
//      //        .append(sqls"""(point, lat, lng) values (PointFromText(${sqlPoint.sql}), ${geoNode.point.latitude}, ${geoNode.point.longitude})""")
//      //      insert.into(GeoNode).namedValues(
//      //        GeoNode.column.point -> sqls"PointFromText(${sqlPoint.sql})",
//      //        sqls"lat" -> geoNode.point.latitude,
//      //        sqls"lng" -> geoNode.point.longitude
//      //      )
//
//      //    }
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
