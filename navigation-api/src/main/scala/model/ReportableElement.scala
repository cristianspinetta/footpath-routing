package model

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.Ramp

sealed trait ReportableElementType
case object RampType extends ReportableElementType
case object SidewalkType extends ReportableElementType

class ReportableElement(id: Long,
                              `type`: ReportableElementType,
                              position: Coordinate,
                              from: Coordinate,
                              to: Coordinate,
                              enabled: Boolean
                            )

object ReportableElement {

  def apply(ramp: Ramp): ReportableElement = new ReportableElement(
    id = ramp.id.get,
    `type` = RampType,
    position = ramp.coordinate,
    enabled = ramp.isAccessible,
    from = null,
    to = null
  )

}

