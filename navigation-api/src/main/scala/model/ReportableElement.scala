package model

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.Ramp

sealed trait ReportableElementType
case object RAMP extends ReportableElementType
case object SIDEWALK extends ReportableElementType

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

}

