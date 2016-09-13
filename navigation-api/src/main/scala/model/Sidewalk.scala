package model

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.{ SidewalkEdge, SidewalkGraphContainer }

case class Sidewalk(from: Coordinate, to: Coordinate)

object Sidewalk {
  def apply(sidewalkEdge: SidewalkEdge)(implicit graphContainer: SidewalkGraphContainer): Sidewalk =
    new Sidewalk(sidewalkEdge.from.get.coordinate, sidewalkEdge.to.get.coordinate)
}
