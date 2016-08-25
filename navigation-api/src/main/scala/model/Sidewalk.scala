package model

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.SidewalkEdge

case class Sidewalk(from: Coordinate, to: Coordinate)

object Sidewalk {
  def apply(sidewalkEdge: SidewalkEdge): Sidewalk =
    new Sidewalk(sidewalkEdge.from.coordinate, sidewalkEdge.to.coordinate)
}
