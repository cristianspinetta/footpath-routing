package model

import mapdomain.graph.{Coordinate, GeoEdge, GeoVertex}
import mapdomain.sidewalk.{SidewalkEdge, SidewalkVertex, StreetCrossingEdge}
import mapdomain.street.{StreetEdge, StreetVertex}

import scala.language.existentials

case class Edge(id: String, from: Coordinate, to: Coordinate, `type`: EdgeType)

sealed trait EdgeType
case object StreetEdgeType extends EdgeType
case object SidewalkEdgeType extends EdgeType
case object StreetCrossingEdgeType extends EdgeType

object EdgeType {

  def create[E <: GeoEdge](edge: E): EdgeType = edge match {
    case _: StreetEdge => StreetEdgeType
    case _: SidewalkEdge => SidewalkEdgeType
    case _: StreetCrossingEdge => StreetCrossingEdgeType
  }
}

case class Vertex(id: Long, coordinate: Coordinate, `type`: VertexType)

object Vertex {
  def createByGeoVertex[V <: GeoVertex[_]](vertex: V): Vertex = new Vertex(vertex.id, vertex.coordinate, VertexType.create(vertex))
}

sealed trait VertexType
case object StreetVertexType extends VertexType
case object SidewalkVertexType extends VertexType

object VertexType {

  def create[V <: GeoVertex[_]](vertex: V): VertexType = vertex match {
    case _: StreetVertex[_] => StreetVertexType
    case _: SidewalkVertex => SidewalkVertexType

  }
}

case class MapContainer(edges: List[Edge], vertices: List[Vertex])
