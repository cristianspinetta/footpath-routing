package mapdomain.publictransport

import scalikejdbc._

case class PublicTransportCombinationPath(
  fromStopId: Long,
  toTravelInfoId: Long,
  walkPath: String
)

object PublicTransportCombinationPath extends SQLSyntaxSupport[PublicTransportCombinationPath] {

  override val tableName = "public_transport_combination_path"
  override val useSnakeCaseColumnName = false

}
