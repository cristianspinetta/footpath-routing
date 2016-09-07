package sql

import mapdomain.graph.Coordinate
import scalikejdbc._

trait SpatialSQLSupport {

  def positionToSQL(coordinate: Coordinate): SQLSyntax = {
    SQLSyntax.createUnsafely(s"PointFromText('POINT(${coordinate.longitude} ${coordinate.latitude})')")
  }
}

object SpatialSQLSupport extends SpatialSQLSupport
