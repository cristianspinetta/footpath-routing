package mapdomain.publictransport

import mapdomain.graph.Coordinate
import scalikejdbc._

case class Stop(
  id: Long,
  coordinate: Coordinate,
  nextStopId: Option[Long] = None,
  previousStopId: Option[Long] = None,
  sequence: Long,
  pathId: Option[Long] = None,
  travelInfoId: Long,
  isAccessible: Boolean)

case class StopUnsaved(
  coordinate: Coordinate,
  nextStopId: Option[Long] = None,
  previousStopId: Option[Long] = None,
  sequence: Long,
  pathId: Option[Long] = None,
  travelInfoId: Long,
  isAccessible: Boolean)

object Stop extends SQLSyntaxSupport[Stop] {

  override val tableName = "stop"
  override val useSnakeCaseColumnName = false

  def createByUnsaved(id: Long, stopUnsaved: StopUnsaved): Stop = Stop(
    id = id,
    coordinate = stopUnsaved.coordinate,
    nextStopId = stopUnsaved.nextStopId,
    previousStopId = stopUnsaved.previousStopId,
    sequence = stopUnsaved.sequence,
    pathId = stopUnsaved.pathId,
    travelInfoId = stopUnsaved.travelInfoId,
    isAccessible = stopUnsaved.isAccessible)
}
