package model

import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex }
import mapdomain.sidewalk.SidewalkEdge
import mapdomain.street.StreetEdge

import scala.language.existentials

case class Edge(id: String, from: Coordinate, to: Coordinate, `type`: EdgeType)

sealed trait EdgeType
case object StreetEdgeType extends EdgeType
case object PedestrianEdgeType extends EdgeType
case object SidewalkEdgeType extends EdgeType
case object StreetCrossingEdgeType extends EdgeType
case object WayEdgeType extends EdgeType
case object WayAreaEdgeType extends EdgeType

object EdgeType {
  val keyMap: Map[String, EdgeType] = Map(
    "street" -> StreetEdgeType,
    "sidewalk" -> SidewalkEdgeType,
    "way" -> WayEdgeType,
    "wayArea" -> WayAreaEdgeType) withDefaultValue StreetEdgeType

  def create[E <: GeoEdge](edge: E): EdgeType = edge match {
    case _: StreetEdge => SidewalkEdgeType
    case _: SidewalkEdge => SidewalkEdgeType
    case _: SidewalkEdge => SidewalkEdgeType
    case _: SidewalkEdge => SidewalkEdgeType

  }
}

case class Vertex(id: Long, coordinate: Coordinate)

object Vertex {
  def createByGeoVertex[V <: GeoVertex[_]](vertex: V): Vertex = new Vertex(vertex.id, vertex.coordinate)
}

case class MapContainer(edges: List[Edge], vertices: List[Vertex])
