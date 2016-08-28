package model

import mapdomain.graph.{ Coordinate, GraphContainer }
import mapdomain.sidewalk.{ SidewalkEdge, SidewalkVertex }

case class Sidewalk(from: Coordinate, to: Coordinate)

object Sidewalk {
  def apply(sidewalkEdge: SidewalkEdge)(implicit graphContainer: GraphContainer[SidewalkVertex]): Sidewalk =
    new Sidewalk(sidewalkEdge.from.get.coordinate, sidewalkEdge.to.get.coordinate)
}
