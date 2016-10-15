package mapdomain.street

import mapdomain.graph.{ BaseEntity, GeoEdge }
import scalikejdbc._

sealed trait StreetEdge extends GeoEdge with BaseEntity {
  val vertexStartId: Long
  val vertexEndId: Long
  val distance: Double
  val wayId: Long
  val streetInfoId: Long
  val id: Option[Long] = None
}

object StreetEdge extends SQLSyntaxSupport[StreetEdge] {

  override val tableName = "street_edge"

  override val columns = Seq("id", "distance", "wayId", "vertexStartId", "streetInfoId", "vertexEndId")

  override val useSnakeCaseColumnName = false

  def create(streetVertexStart: StreetVertex[StreetEdge], streetVertexEnd: StreetVertex[StreetEdge], distance: Double, wayId: Long, streetInfoId: Long): StreetEdge =
    StreetEdgeSaved(streetVertexStart.id, streetVertexEnd.id, distance, wayId, streetInfoId)

  def create(id: Option[Long] = None, streetVertexStartId: Long, streetVertexEndId: Long, distance: Double, wayId: Long, streetInfoId: Long): StreetEdge = new StreetEdgeSaved(streetVertexStartId, streetVertexEndId, distance, wayId, streetInfoId, id)

  def apply(vertexStartId: Long, vertexEndId: Long, distance: Double, wayId: Long, streetInfoId: Long, id: Option[Long] = None): StreetEdge = {
    StreetEdgeSaved(vertexStartId, vertexEndId, distance, wayId, streetInfoId, id)
  }
}

case class StreetEdgeSaved(vertexStartId: Long, vertexEndId: Long, distance: Double, wayId: Long, streetInfoId: Long, override val id: Option[Long] = None)
  extends StreetEdge

case class StreetEdgeUnsaved(vertexStartId: Long, vertexEndId: Long, distance: Double, wayId: Long, streetInfo: StreetInfo)
    extends StreetEdge {

  override val streetInfoId: Long = streetInfo.id.getOrElse(0)

  def build(streetInfoId: Long): StreetEdge = StreetEdge.create(None, vertexStartId, vertexEndId, distance, wayId, streetInfoId)
}

case class StreetInfo(id: Option[Long] = None, address: Option[String], wayId: Long)

object StreetInfo extends SQLSyntaxSupport[StreetInfo] {

  override val tableName = "street_info"

  override val columns = Seq("id", "address", "wayId")

  override val useSnakeCaseColumnName = false
}
