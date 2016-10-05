package mapdomain.publictransport

import mapdomain.graph.Coordinate
import scalikejdbc._

case class Stop(
  id: Option[Long] = None,
  coordinate: Coordinate,
  nextStopId: Option[Long] = None,
  previousStopId: Option[Long] = None,
  pathId: Option[Long] = None,
  travelInfoId: Option[Long] = None,
  isAccessible: Boolean)

object Stop extends SQLSyntaxSupport[Stop] {

  override val tableName = "stop"

  override val useSnakeCaseColumnName = false

}
