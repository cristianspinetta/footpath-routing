package model

import mapdomain.graph.Coordinate
import mapdomain.repository.sidewalk.SidewalkVertexRepository
import mapdomain.sidewalk.{ Ramp, SidewalkEdge }

sealed trait ReportableElementType
case object RAMP extends ReportableElementType
case object SIDEWALK extends ReportableElementType
case object STOP extends ReportableElementType

case class ReportableElement(id: Long,
  `type`: ReportableElementType,
  position: Option[Coordinate] = None,
  from: Option[Coordinate] = None,
  to: Option[Coordinate] = None,
  enabled: Boolean)

object ReportableElement {

  def apply(element: ReportableElement) = new ReportableElement(
    element.id, element.`type`, element.position, element.from, element.to, element.enabled)

  def apply(ramp: Ramp): ReportableElement = new ReportableElement(
    id = ramp.id.get,
    `type` = RAMP,
    position = Some(ramp.coordinate),
    enabled = ramp.isAccessible)

  def apply(edge: SidewalkEdge): ReportableElement = new ReportableElement(
    id = edge.id.get,
    `type` = SIDEWALK,
    from = Some(SidewalkVertexRepository.find(edge.vertexStartId).get.coordinate),
    to = Some(SidewalkVertexRepository.find(edge.vertexEndId).get.coordinate),
    enabled = edge.isAccessible)

  def apply(stop: mapdomain.publictransport.Stop): ReportableElement = new ReportableElement(
    id = stop.id,
    `type` = STOP,
    position = Some(stop.coordinate),
    enabled = stop.isAccessible)

}

