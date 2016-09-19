package model

import mapdomain.graph.Coordinate

case class Edge(id: String, from: Coordinate, to: Coordinate)

trait EdgeType
case object StreetEdgeType extends EdgeType
case object SidewalkEdgeType extends EdgeType
case object WayEdgeType extends EdgeType
case object WayAreaEdgeType extends EdgeType

object EdgeType {
  def apply(edgeType: String): EdgeType = edgeType match {
    case "street"   ⇒ StreetEdgeType
    case "sidewalk" ⇒ SidewalkEdgeType
    case "way"      ⇒ WayEdgeType
    case "wayArea"  ⇒ WayAreaEdgeType
    case _          ⇒ StreetEdgeType
  }
}
