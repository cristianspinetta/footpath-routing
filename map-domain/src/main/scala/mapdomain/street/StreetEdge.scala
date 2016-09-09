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

trait StreetEdgeRepository {

  val (e, v) = (StreetEdge.syntax("e"), StreetVertex.syntax("v"))

  def streetEdge(e: SyntaxProvider[StreetEdge])(rs: WrappedResultSet): StreetEdge = streetEdge(e.resultName)(rs)

  private def streetEdge(e: ResultName[StreetEdge])(implicit rs: WrappedResultSet): StreetEdge = {
    new StreetEdge(
      rs.long(e.vertexStartId),
      rs.long(e.vertexEndId),
      rs.double(e.distance),
      rs.long(e.wayId),
      Some(rs.long(e.id)))
  }

  def opt(e: SyntaxProvider[StreetEdge])(rs: WrappedResultSet): Option[StreetEdge] =
    rs.longOpt(e.resultName.id).map(_ ⇒ streetEdge(e)(rs))

  def create(edge: StreetEdge)(implicit session: DBSession = StreetEdge.autoSession): Long = {
    withSQL {
      insert.into(StreetEdge).namedValues(
        StreetEdge.column.vertexStartId -> edge.vertexStartId,
        StreetEdge.column.vertexEndId -> edge.vertexEndId,
        StreetEdge.column.distance -> edge.distance,
        StreetEdge.column.wayId -> edge.wayId)
    }.updateAndReturnGeneratedKey.apply()
  }

  def find(id: Long)(implicit session: DBSession = StreetEdge.autoSession): StreetEdge = withSQL {
    select.
      from(StreetEdge as e)
      .where.eq(e.id, id)
  }.map(streetEdge(e)).single().apply().get

  def deleteAll(implicit session: DBSession = StreetEdge.autoSession): Unit = withSQL {
    deleteFrom(StreetEdge)
  }.update.apply()

  def findStreetEdgesByStreetVertex(vertexId: Long)(implicit session: DBSession = StreetEdge.autoSession): List[StreetEdge] = DB readOnly { implicit session ⇒
    sql"""
       select
        ${e.result.*}
       from
        ${StreetVertex.as(v)}
        left join ${StreetEdge.as(e)} on ${e.vertexStartId} = ${v.id}
       where
        ${v.id} = ${vertexId}
    """.map(StreetEdgeRepository.streetEdge(e))
      .list.apply()
  }

}

object StreetEdgeRepository extends StreetEdgeRepository
