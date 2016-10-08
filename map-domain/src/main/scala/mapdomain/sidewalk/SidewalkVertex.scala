package mapdomain.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.graph._
import scalikejdbc._

trait SidewalkVertex extends GeoVertex[PedestrianEdge] {

  val coordinate: Coordinate
  val sidewalkEdges: List[SidewalkEdge]
  val streetCrossingEdges: List[StreetCrossingEdge]
  val streetVertexBelongToId: Long

  val edges: List[PedestrianEdge] = sidewalkEdges ++ streetCrossingEdges

  override def getEdgesFor(vertexId: Long): Option[PedestrianEdge] = edges.find(edge ⇒ edge.vertexEndId == vertexId || edge.vertexStartId == vertexId)

  def copy(id: Long = id, coordinate: Coordinate = coordinate, sidewalkEdges: List[SidewalkEdge] = sidewalkEdges,
    streetCrossingEdges: List[StreetCrossingEdge] = streetCrossingEdges, streetVertexBelongToId: Long = streetVertexBelongToId): SidewalkVertex = {
    SidewalkVertex(id, coordinate, sidewalkEdges, streetCrossingEdges, streetVertexBelongToId)
  }
}

object SidewalkVertex extends SQLSyntaxSupport[SidewalkVertex] {
  override val tableName = "sidewalk_vertex"
  override val useSnakeCaseColumnName = false

  def apply(id: Long, coordinate: Coordinate, sidewalkEdges: List[SidewalkEdge],
    streetCrossingEdges: List[StreetCrossingEdge], streetVertexBelongToId: Long): SidewalkVertex = {
    new SidewalkVertexImpl(id, coordinate, sidewalkEdges, streetCrossingEdges, streetVertexBelongToId)
  }
}

class SidewalkVertexImpl(val id: Long, val coordinate: Coordinate, val sidewalkEdges: List[SidewalkEdge],
  val streetCrossingEdges: List[StreetCrossingEdge], val streetVertexBelongToId: Long) extends SidewalkVertex
