package mapdomain.street

import mapdomain.graph.GeoEdge
import scalikejdbc._

class OsmStreetEdge(override val vertexStartId: Long, override val vertexEndId: Long, override val distance: Double, val wayId: Long, val id: Option[Long] = None)
  extends GeoEdge(vertexStartId, vertexEndId, distance, directed = true)

object OsmStreetEdge extends SQLSyntaxSupport[OsmStreetEdge] {

  override val tableName = "OsmStreetEdge"

  override val columns = Seq("id", "distance", "wayId", "vertexStartId", "vertexEndId")

  override val useSnakeCaseColumnName = false

  def apply(osmVertexStart: OsmVertex, osmVertexEnd: OsmVertex, distance: Double, wayId: Long): OsmStreetEdge =
    new OsmStreetEdge(osmVertexStart.id, osmVertexEnd.id, distance, wayId)

  def apply(id: Option[Long] = None, osmVertexStartId: Long, osmVertexEndId: Long, distance: Double, wayId: Long): OsmStreetEdge = new OsmStreetEdge(osmVertexStartId, osmVertexEndId, distance, wayId, id)

}

trait OsmStreetEdgeRepository {

  val (e, v) = (OsmStreetEdge.syntax("e"), OsmVertex.syntax("v"))

  def osmStreetEdge(e: SyntaxProvider[OsmStreetEdge])(rs: WrappedResultSet): OsmStreetEdge = osmStreetEdge(e.resultName)(rs)

  private def osmStreetEdge(e: ResultName[OsmStreetEdge])(implicit rs: WrappedResultSet): OsmStreetEdge = {
    new OsmStreetEdge(
      rs.long(e.vertexStartId),
      rs.long(e.vertexEndId),
      rs.double(e.distance),
      rs.long(e.wayId),
      Some(rs.long(e.id)))
  }

  def opt(e: SyntaxProvider[OsmStreetEdge])(rs: WrappedResultSet): Option[OsmStreetEdge] =
    rs.longOpt(e.resultName.id).map(_ ⇒ osmStreetEdge(e)(rs))

  def create(edge: OsmStreetEdge)(implicit session: DBSession = OsmStreetEdge.autoSession): Long = {
    withSQL {
      insert.into(OsmStreetEdge).namedValues(
        OsmStreetEdge.column.vertexStartId -> edge.vertexStartId,
        OsmStreetEdge.column.vertexEndId -> edge.vertexEndId,
        OsmStreetEdge.column.distance -> edge.distance,
        OsmStreetEdge.column.wayId -> edge.wayId)
    }.updateAndReturnGeneratedKey.apply()
  }

  def find(id: Long)(implicit session: DBSession = OsmStreetEdge.autoSession): OsmStreetEdge = withSQL {
    select.
      from(OsmStreetEdge as e)
      .where.eq(e.id, id)
  }.map(osmStreetEdge(e)).single().apply().get

  def deleteAll(implicit session: DBSession = OsmStreetEdge.autoSession): Unit = withSQL {
    deleteFrom(OsmStreetEdge)
  }.update.apply()

  def findStreetEdgesByStreetVertex(vertexId: Long)(implicit session: DBSession = OsmStreetEdge.autoSession): List[OsmStreetEdge] = DB readOnly { implicit session ⇒
    sql"""
       select
        ${e.result.*}
       from
        ${OsmVertex.as(v)}
        left join ${OsmStreetEdge.as(e)} on ${e.vertexStartId} = ${v.id}
       where
        ${v.id} = ${vertexId}
    """.map(OsmStreetEdgeRepository.osmStreetEdge(e))
      .list.apply()
  }

}

object OsmStreetEdgeRepository extends OsmStreetEdgeRepository
