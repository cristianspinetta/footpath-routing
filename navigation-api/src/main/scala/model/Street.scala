package model

import mapdomain.graph.{ Coordinate, GeoVertex, GraphContainer }
import mapdomain.street.StreetEdge

case class Street(id: String, from: Coordinate, to: Coordinate)

object Street {
  def apply[V <: GeoVertex](streetEdge: StreetEdge)(implicit graph: GraphContainer[V]): Street =
    new Street(streetEdge.wayId.toString, graph.findVertex(streetEdge.vertexStartId).get.coordinate,
      graph.findVertex(streetEdge.vertexEndId).get.coordinate)
}
