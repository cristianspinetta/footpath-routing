package mapdomain.publictransport

import scalikejdbc._

case class TravelInfo(
    id: Long,
    description: String,
    firstStopId: Long,
    lastStopId: Long,
    branch: String,
    name: String,
    sentido: String,
    `type`: String) {
  val title: String = s"$name-$branch-$sentido (${`type`})"
}

case class TravelInfoUnsaved(
  description: String,
  firstStopId: Option[Long] = None,
  lastStopId: Option[Long] = None,
  branch: String,
  name: String,
  sentido: String,
  `type`: String)

object TravelInfo extends SQLSyntaxSupport[TravelInfo] {

  override val tableName = "travel_info"

  override val useSnakeCaseColumnName = false

  def createByUnsaved(id: Long, travelInfoUnsaved: TravelInfoUnsaved): TravelInfo = {
    TravelInfo(
      id = id,
      description = travelInfoUnsaved.description,
      firstStopId = travelInfoUnsaved.firstStopId.get,
      lastStopId = travelInfoUnsaved.lastStopId.get,
      branch = travelInfoUnsaved.branch,
      name = travelInfoUnsaved.name,
      sentido = travelInfoUnsaved.sentido,
      `type` = travelInfoUnsaved.`type`)
  }

}

