package mapgenerator.source.osm.graph

import mapgenerator.source.osm.{ OSMNode, Way }
import pathgenerator.graph.{ Coordinate, GeoEdge, GeoVertex }

class OsmVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends GeoVertex(id, edges, coordinate)

object OsmVertex {
  def apply(way: Way, node: OSMNode): OsmVertex = new OsmVertex(node.id, Nil, Coordinate(node.lat, node.lon))
}

case class TransitStopStreetVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)
case class ExitVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate, exitName: String) extends OsmVertex(id, edges, coordinate)
case class BarrierVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)

case class OsmStreetEdge(osmVertexStart: OsmVertex, osmVertexEnd: OsmVertex, override val distance: Double)
  extends GeoEdge(osmVertexStart.id, osmVertexEnd.id, distance, directed = true)
