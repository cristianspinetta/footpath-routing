package sql

import mapdomain.graph.Coordinate
import mapdomain.publictransport.Stop
import scalikejdbc._

trait SpatialSQLSupport {

  def positionToSQL(coordinate: Coordinate): SQLSyntax = {
    SQLSyntax.createUnsafely(s"PointFromText('POINT(${coordinate.longitude} ${coordinate.latitude})')")
  }

  def selectLatitudeAndLongitude(s: SyntaxProvider[_]): SQLSyntax = {
    val lng = SQLSyntax.createUnsafely(s"${s.tableAliasName}_lng")
    val lat = SQLSyntax.createUnsafely(s"${s.tableAliasName}_lat")
    sqls", x(${s.column("coordinate")}) $lng, y(${s.column("coordinate")}) $lat"
  }

  def coordinateFromResultSet(rs: WrappedResultSet, tableAlias: String): Coordinate = {
    Coordinate(rs.double(s"${tableAlias}_lat"), rs.double(s"${tableAlias}_lng"))
  }

}

object SpatialSQLSupport extends SpatialSQLSupport
