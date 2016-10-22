package searching

import mapdomain.graph.Coordinate

case class PedestrianIncident(`type`: IncidentType,
  position: Option[Coordinate] = None,
  from: Option[Coordinate] = None,
  to: Option[Coordinate] = None)

sealed trait IncidentType
case object SidewalkIncidentType extends IncidentType
case object RampIncidentType extends IncidentType
