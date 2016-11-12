package mapdomain.publictransport

import mapdomain.graph.Coordinate
import scalikejdbc._

case class PublicTransportCombination(
  fromStopId: Long,
  fromCoordinate: Coordinate,
  toStopId: Long,
  toCoordinate: Coordinate,
  fromTravelInfoId: Long,
  toTravelInfoId: Long,
  distance: Double,
  walkPath: Option[String] = None,
  enabled: Boolean,
  cost: Double)

object PublicTransportCombination extends SQLSyntaxSupport[PublicTransportCombination] {

  override val tableName = "public_transport_combination"
  override val useSnakeCaseColumnName = false
}
