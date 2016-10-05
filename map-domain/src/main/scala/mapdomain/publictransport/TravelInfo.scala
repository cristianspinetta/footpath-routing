package mapdomain.publictransport

import scalikejdbc._

case class TravelInfo(
  id: Option[Long] = None,
  description: String,
  firstStopId: Option[Long] = None,
  lastStopId: Option[Long] = None)

object TravelInfo extends SQLSyntaxSupport[TravelInfo] {

  override val tableName = "travel_info"

  override val useSnakeCaseColumnName = false

}

