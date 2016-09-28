package mapdomain.street

import mapdomain.graph.{ BaseEntity, GeoEdge }
import scalikejdbc._

class StreetEdge(override val vertexStartId: Long, override val vertexEndId: Long, override val distance: Double, val wayId: Long, val streetInfoId: Long, val id: Option[Long] = None)
  extends GeoEdge(vertexStartId, vertexEndId, distance, directed = true) with BaseEntity

object StreetEdge extends SQLSyntaxSupport[StreetEdge] {

  override val tableName = "street_edge"

  override val columns = Seq("id", "distance", "wayId", "vertexStartId", "streetInfoId", "vertexEndId")

  override val useSnakeCaseColumnName = false

  def apply(streetVertexStart: StreetVertex.T, streetVertexEnd: StreetVertex.T, distance: Double, wayId: Long, streetInfoId: Long): StreetEdge =
    new StreetEdge(streetVertexStart.id, streetVertexEnd.id, distance, wayId, streetInfoId)

  def apply(id: Option[Long] = None, streetVertexStartId: Long, streetVertexEndId: Long, distance: Double, wayId: Long, streetInfoId: Long): StreetEdge = new StreetEdge(streetVertexStartId, streetVertexEndId, distance, wayId, streetInfoId, id)

}

case class StreetEdgeUnsaved(override val vertexStartId: Long, override val vertexEndId: Long, override val distance: Double, override val wayId: Long, streetInfo: StreetInfo)
    extends StreetEdge(vertexStartId, vertexEndId, distance, wayId, streetInfo.id.getOrElse(0)) {
  def build(streetInfoId: Long): StreetEdge = StreetEdge(None, vertexStartId, vertexEndId, distance, wayId, streetInfoId)
}

case class StreetInfo(id: Option[Long] = None, address: Option[String], wayId: Long)

object StreetInfo extends SQLSyntaxSupport[StreetInfo] {

  override val tableName = "street_info"

  override val columns = Seq("id", "address", "wayId")

  override val useSnakeCaseColumnName = false
}
