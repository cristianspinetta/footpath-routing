package model

import mapdomain.graph.{Coordinate, GeoEdge, GeoVertex, GraphContainer}
import mapdomain.street.StreetEdge

case class Street(id: String, from: Coordinate, to: Coordinate)

object Street {
  def apply[E <: GeoEdge, V <: GeoVertex[E]](streetEdge: StreetEdge)(implicit graph: GraphContainer[E, V]): Street =
    new Street(streetEdge.wayId.toString, graph.findVertex(streetEdge.vertexStartId).get.coordinate,
      graph.findVertex(streetEdge.vertexEndId).get.coordinate)
}
