package mapdomain.street

import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex }

class OsmVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends GeoVertex(id, edges, coordinate) {
  override def toString = s"OsmVertex($id, $edges, $coordinate)"
}

case class TransitStopStreetVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)
case class ExitVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate, exitName: String) extends OsmVertex(id, edges, coordinate)
case class BarrierVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)

case class OsmStreetEdge(osmVertexStart: OsmVertex, osmVertexEnd: OsmVertex, override val distance: Double, wayId: Long)
  extends GeoEdge(osmVertexStart.id, osmVertexEnd.id, distance, directed = true)
