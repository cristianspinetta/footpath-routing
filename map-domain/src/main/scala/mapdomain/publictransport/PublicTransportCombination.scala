package mapdomain.publictransport

import scalikejdbc._

case class PublicTransportCombination(
  fromStopId: Long,
  toStopId: Long,
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
