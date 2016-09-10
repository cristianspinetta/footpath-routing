package mapdomain.street

import mapdomain.graph.GeoEdge
import scalikejdbc._

class StreetEdge(override val vertexStartId: Long, override val vertexEndId: Long, override val distance: Double, val wayId: Long, val id: Option[Long] = None)
  extends GeoEdge(vertexStartId, vertexEndId, distance, directed = true)

object StreetEdge extends SQLSyntaxSupport[StreetEdge] {

  override val tableName = "street_edge"

  override val columns = Seq("id", "distance", "wayId", "vertexStartId", "vertexEndId")

  override val useSnakeCaseColumnName = false

  def apply(streetVertexStart: StreetVertex, streetVertexEnd: StreetVertex, distance: Double, wayId: Long): StreetEdge =
    new StreetEdge(streetVertexStart.id, streetVertexEnd.id, distance, wayId)

  def apply(id: Option[Long] = None, streetVertexStartId: Long, streetVertexEndId: Long, distance: Double, wayId: Long): StreetEdge = new StreetEdge(streetVertexStartId, streetVertexEndId, distance, wayId, id)

}
