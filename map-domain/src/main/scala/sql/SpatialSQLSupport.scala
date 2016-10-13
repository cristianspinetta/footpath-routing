package sql

import mapdomain.graph.{ BoundedGeoLocation, Coordinate }
import scalikejdbc._

trait SpatialSQLSupport {

  def positionToSQL(coordinate: Coordinate): SQLSyntax = {
    SQLSyntax.createUnsafely(s"PointFromText('POINT(${coordinate.radLongitude} ${coordinate.radLatitude})')")
  }

  def distance(coordinate: Coordinate, s: SyntaxProvider[_], positionColumnName: String): SQLSyntax = {
    SQLSyntax.createUnsafely(s"ST_Distance(point(${coordinate.radLongitude},${coordinate.radLatitude}), ${s.column(positionColumnName).value})")
  }

  def orderByDistance(coordinate: Coordinate, s: SyntaxProvider[_], positionColumnName: String) = {
    sqls"order by ${distance(coordinate, s, positionColumnName)}"
  }

  def selectLatitudeAndLongitude(s: SyntaxProvider[_]): SQLSyntax = {
    val lng = SQLSyntax.createUnsafely(s"${s.tableAliasName}_lng")
    val lat = SQLSyntax.createUnsafely(s"${s.tableAliasName}_lat")
    sqls", x(${s.column("coordinate")}) $lng, y(${s.column("coordinate")}) $lat"
  }

  def coordinateFromResultSet(rs: WrappedResultSet, tableAlias: String): Coordinate = {
    Coordinate.byRadians(rs.double(s"${tableAlias}_lat"), rs.double(s"${tableAlias}_lng"))
  }

  def clauseNearestByDistance(coordinate: Coordinate, radius: Double, s: SyntaxProvider[_], positionColumnName: String): SQLSyntax = {
    // angular distance in radians on a great circle
    val radDistance = radius / Coordinate.radius
    val bound: BoundedGeoLocation = BoundedGeoLocation.boundByDistance(coordinate, radius)

    if (bound.meridian180WithinDistance)
      sqls"""
         | (y(${s.column(positionColumnName)}) >= ${bound.min.radLatitude} and y(${s.column(positionColumnName)}) <= ${bound.max.radLatitude})
         | and (x(${s.column(positionColumnName)}) >= ${bound.min.radLongitude} or x(${s.column(positionColumnName)}) <= ${bound.max.radLongitude})
         | and ${distance(coordinate, s, positionColumnName)} <= $radDistance""".stripMargin
    else
      sqls"""
         | (y(${s.column(positionColumnName)}) >= ${bound.min.radLatitude} and y(${s.column(positionColumnName)}) <= ${bound.max.radLatitude})
         | and (x(${s.column(positionColumnName)}) >= ${bound.min.radLongitude} and x(${s.column(positionColumnName)}) <= ${bound.max.radLongitude})
         | and ${distance(coordinate, s, positionColumnName)} <= $radDistance""".stripMargin
  }

  def clauseGetElementsInRectangle(northEast: Coordinate, southWest: Coordinate, s: SyntaxProvider[_], positionColumnName: String): SQLSyntax = {
    sqls"""
     y(${s.column(positionColumnName)}) >= ${southWest.radLatitude}
     and y(${s.column(positionColumnName)}) <= ${northEast.radLatitude}
     and x(${s.column(positionColumnName)}) >= ${southWest.radLongitude}
     and x(${s.column(positionColumnName)}) <= ${northEast.radLongitude}""".stripMargin
  }

}

object SpatialSQLSupport extends SpatialSQLSupport
