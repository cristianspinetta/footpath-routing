package searching

import mapdomain.graph.Coordinate
import org.json4s.CustomSerializer
import org.json4s.JsonAST.JString

case class PedestrianIncident(`type`: IncidentType,
  position: Option[Coordinate] = None,
  from: Option[Coordinate] = None,
  to: Option[Coordinate] = None)

sealed trait IncidentType
case object SidewalkIncidentType extends IncidentType
case object RampIncidentType extends IncidentType

object IncidentType {
  val keyMap: Map[String, IncidentType] = Map(
    "sidewalk" -> SidewalkIncidentType,
    "ramp" -> RampIncidentType) withDefaultValue SidewalkIncidentType
}

case object IncidentTypeSerializer extends CustomSerializer[IncidentType](format ⇒ ({
  case JString(s) ⇒ IncidentType.keyMap(s)
}, {
  case incidentType: IncidentType ⇒ JString(IncidentType.keyMap.find(i ⇒ incidentType.equals(i._2)).get._1)
}))
