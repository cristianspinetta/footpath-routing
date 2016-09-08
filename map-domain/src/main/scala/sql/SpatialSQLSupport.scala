package sql

import mapdomain.graph.Coordinate
import mapdomain.publictransport.Stop
import scalikejdbc._

trait SpatialSQLSupport {

  def positionToSQL(coordinate: Coordinate): SQLSyntax = {
    SQLSyntax.createUnsafely(s"PointFromText('POINT(${coordinate.longitude} ${coordinate.latitude})')")
  }

  def selectLatitudeAndLongitude(s: SyntaxProvider[Stop]): SQLSyntax = {
    val lng = SQLSyntax.createUnsafely(s"${s.tableAliasName}_lng")
    val lat = SQLSyntax.createUnsafely(s"${s.tableAliasName}_lat")
    sqls", x(${s.column("coordinate")}) $lng, y(${s.column("coordinate")}) $lat"
  }


}

object SpatialSQLSupport extends SpatialSQLSupport
