package mapdomain.publictransport

import mapdomain.graph.Coordinate
import scalikejdbc._

case class Stop(
  id: Option[Long] = None,
  coordinate: Coordinate,
  nextStopId: Option[Long] = None,
  nextStop: Option[Stop] = None,
  previousStopId: Option[Long] = None,
  previousStop: Option[Stop] = None,
  pathId: Option[Long] = None,
  path: Option[Path] = None,
  travelInfoId: Option[Long] = None,
  travelInfo: Option[TravelInfo] = None,
  isAccessible: Boolean)

object Stop extends SQLSyntaxSupport[Stop] {

  override val tableName = "stop"

  override val useSnakeCaseColumnName = false

}
