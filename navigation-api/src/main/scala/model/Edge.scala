package model

import mapdomain.graph.Coordinate

case class Edge(id: String, from: Coordinate, to: Coordinate)

sealed trait EdgeType
case object StreetEdgeType extends EdgeType
case object SidewalkEdgeType extends EdgeType
case object WayEdgeType extends EdgeType
case object WayAreaEdgeType extends EdgeType

object EdgeType {
  val keyMap: Map[String, EdgeType] = Map(
    "street" -> StreetEdgeType,
    "sidewalk" -> SidewalkEdgeType,
    "way" -> WayEdgeType,
    "wayArea" -> WayAreaEdgeType) withDefaultValue StreetEdgeType
}
