package model

import mapdomain.graph.Coordinate
import mapdomain.street.OsmStreetEdge

case class Street(id: String, from: Coordinate, to: Coordinate)

object Street {
  def apply(osmStreetEdge: OsmStreetEdge): Street =
    new Street(osmStreetEdge.wayId.toString, osmStreetEdge.osmVertexStart.coordinate, osmStreetEdge.osmVertexEnd.coordinate)
}
