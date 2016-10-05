package mapdomain.repository.street

import mapdomain.graph.Coordinate
import mapdomain.street.{ StreetEdge, StreetVertex }
import scalikejdbc._
import sql.SpatialSQLSupport

trait StreetEdgeRepository extends SpatialSQLSupport {

  val (e, v) = (StreetEdge.syntax("e"), StreetVertex.syntax("v"))

  def streetEdge(e: SyntaxProvider[StreetEdge])(rs: WrappedResultSet): StreetEdge = streetEdge(e.resultName)(rs)

  private def streetEdge(e: ResultName[StreetEdge])(implicit rs: WrappedResultSet): StreetEdge = {
    new StreetEdge(
      rs.long(e.vertexStartId),
      rs.long(e.vertexEndId),
      rs.double(e.distance),
      rs.long(e.wayId),
      rs.long(e.streetInfoId),
      Some(rs.long(e.id)))
  }

  def create(edge: StreetEdge)(implicit session: DBSession = StreetEdge.autoSession): Long = {
    withSQL {
      insert.into(StreetEdge).namedValues(
        StreetEdge.column.vertexStartId -> edge.vertexStartId,
        StreetEdge.column.vertexEndId -> edge.vertexEndId,
        StreetEdge.column.distance -> edge.distance,
        StreetEdge.column.wayId -> edge.wayId,
        StreetEdge.column.streetInfoId -> edge.streetInfoId)
    }.updateAndReturnGeneratedKey.apply()
  }

  def find(id: Long)(implicit session: DBSession = StreetEdge.autoSession): StreetEdge = withSQL {
    select.
      from(StreetEdge as e)
      .where.eq(e.id, id)
  }.map(streetEdge(e)).single().apply().get

  def findNearestStreets(coordinate: Coordinate, radius: Double)(implicit session: DBSession = StreetEdge.autoSession): List[StreetEdge] = withSQL {
    select
      .from(StreetEdge as e)
      .leftJoin(StreetVertex as v).on(e.vertexStartId, v.id) // FIXME hacer join con vertices end, agregando un on en el left join
      .where.append(clauseNearestByDistance(coordinate, radius, v, "coordinate"))
  }.map(streetEdge(e)).list().apply()

  def deleteAll(implicit session: DBSession = StreetEdge.autoSession): Unit = withSQL {
    deleteFrom(StreetEdge)
  }.update.apply()

  def findStreetEdgesByStreetVertex(vertexId: Long)(implicit session: DBSession = StreetEdge.autoSession): List[StreetEdge] = withSQL {
    select.
      from(StreetEdge as e)
      .where.eq(e.vertexStartId, vertexId)
  }.map(streetEdge(e)).list().apply()

}

object StreetEdgeRepository extends StreetEdgeRepository
