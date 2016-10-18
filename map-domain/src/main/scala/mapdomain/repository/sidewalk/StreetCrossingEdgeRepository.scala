package mapdomain.repository.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.{ SidewalkEdge, SidewalkVertex, StreetCrossingEdge }
import scalikejdbc._
import sql.SpatialSQLSupport

trait StreetCrossingEdgeRepository extends SpatialSQLSupport {

  val (sce, sv) = (StreetCrossingEdge.syntax("sce"), SidewalkVertex.syntax("sv"))

  def streetCrossingEdge(e: SyntaxProvider[StreetCrossingEdge])(rs: WrappedResultSet): StreetCrossingEdge = streetCrossingEdge(e.resultName)(rs)

  private def streetCrossingEdge(e: ResultName[StreetCrossingEdge])(implicit rs: WrappedResultSet): StreetCrossingEdge = {
    new StreetCrossingEdge(
      vertexStartId = rs.long(e.vertexStartId),
      vertexEndId = rs.long(e.vertexEndId),
      keyValue = rs.string(e.keyValue),
      id = rs.longOpt(e.id),
      rampStartId = rs.longOpt(e.rampStartId),
      rampEndId = rs.longOpt(e.rampEndId))
  }

  def opt(e: SyntaxProvider[StreetCrossingEdge])(rs: WrappedResultSet): Option[StreetCrossingEdge] =
    rs.longOpt(e.resultName.id).map(_ ⇒ streetCrossingEdge(e)(rs))

  def create(edge: StreetCrossingEdge)(implicit session: DBSession = StreetCrossingEdge.autoSession): Long = {
    withSQL {
      insert.into(StreetCrossingEdge).namedValues(
        StreetCrossingEdge.column.vertexStartId -> edge.vertexStartId,
        StreetCrossingEdge.column.vertexEndId -> edge.vertexEndId,
        StreetCrossingEdge.column.keyValue -> edge.keyValue,
        StreetCrossingEdge.column.rampStartId -> edge.rampStartId,
        StreetCrossingEdge.column.rampEndId -> edge.rampEndId)
    }.updateAndReturnGeneratedKey.apply()
  }

  def find(id: Long)(implicit session: DBSession = StreetCrossingEdge.autoSession): StreetCrossingEdge = withSQL {
    select.
      from(StreetCrossingEdge as sce)
      .where.eq(sce.id, id)
  }.map(streetCrossingEdge(sce)).single().apply().get

  def findNearestSidewalks(coordinate: Coordinate, radius: Double)(implicit session: DBSession = SidewalkEdge.autoSession): List[StreetCrossingEdge] = withSQL {
    select
      .from(StreetCrossingEdge as sce)
      .leftJoin(SidewalkVertex as sv).on(sce.vertexStartId, sv.id) // FIXME hacer join con vertices end, agregando un on en el left join
      .where.append(clauseNearestByDistance(coordinate, radius, sv, "coordinate"))
  }.map(streetCrossingEdge(sce)).list().apply()

  def deleteAll(implicit session: DBSession = StreetCrossingEdge.autoSession): Unit = withSQL {
    deleteFrom(StreetCrossingEdge)
  }.update.apply()

  def findCrossingEdgesBySidewalkVertex(vertexId: Long)(implicit session: DBSession = StreetCrossingEdge.autoSession): List[StreetCrossingEdge] = DB readOnly { implicit session ⇒
    withSQL {
      select
        .from(StreetCrossingEdge as sce)
        .where.eq(sce.vertexStartId, vertexId).or.eq(sce.vertexEndId, vertexId)
    }.map(streetCrossingEdge(sce)).list.apply()
  }

  def findCrossingEdgesByRamp(rampId: Long)(implicit session: DBSession = StreetCrossingEdge.autoSession): List[StreetCrossingEdge] = DB readOnly { implicit session ⇒
    withSQL {
      select
        .from(StreetCrossingEdge as sce)
        .where.eq(sce.rampStartId, rampId).or.eq(sce.rampEndId, rampId)
    }.map(streetCrossingEdge(sce)).list.apply()
  }

  def deleteStartRamp(crossingEdgeId: Long)(implicit session: DBSession = StreetCrossingEdge.autoSession): Unit = withSQL {
    update(StreetCrossingEdge).set(
      StreetCrossingEdge.column.rampStartId -> null)
  }.update().apply()

  def save(edge: StreetCrossingEdge)(implicit session: DBSession = StreetCrossingEdge.autoSession): StreetCrossingEdge = {
    withSQL {
      update(StreetCrossingEdge).set(
        StreetCrossingEdge.column.vertexStartId -> edge.vertexStartId,
        StreetCrossingEdge.column.vertexEndId -> edge.vertexEndId,
        StreetCrossingEdge.column.keyValue -> edge.keyValue,
        StreetCrossingEdge.column.rampStartId -> edge.rampStartId,
        StreetCrossingEdge.column.rampEndId -> edge.rampEndId
      ).where.eq(StreetCrossingEdge.column.id, edge.id)
    }.update.apply()
    edge
  }

}

object StreetCrossingEdgeRepository extends StreetCrossingEdgeRepository