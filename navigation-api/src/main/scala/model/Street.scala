package model

import mapdomain.graph.{ Coordinate, GeoVertex, GraphContainer }
import mapdomain.street.OsmStreetEdge

case class Street(id: String, from: Coordinate, to: Coordinate)

object Street {
  def apply[V <: GeoVertex](osmStreetEdge: OsmStreetEdge)(implicit graph: GraphContainer[V]): Street =
    new Street(osmStreetEdge.wayId.toString, graph.findVertex(osmStreetEdge.vertexStartId).get.coordinate,
      graph.findVertex(osmStreetEdge.vertexEndId).get.coordinate)
}
