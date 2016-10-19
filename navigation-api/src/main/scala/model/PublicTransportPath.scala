package model

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{ Stop ⇒ DStop }

case class PublicTransportPath(id: Long, description: String, coordinates: List[Coordinate], stops: List[Stop])

case class Stop(
  id: Long,
  coordinate: Coordinate,
  sequence: Long,
  isAccessible: Boolean)

object Stop {

  def createByDomainStop(stop: DStop): Stop = Stop(stop.id, stop.coordinate, stop.sequence, stop.isAccessible)
}
