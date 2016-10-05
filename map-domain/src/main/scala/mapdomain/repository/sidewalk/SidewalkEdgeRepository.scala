package mapdomain.repository.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.sidewalk._
import scalikejdbc._
import sql.SpatialSQLSupport

trait SidewalkEdgeRepository extends SpatialSQLSupport {

  val (se, sv) = (SidewalkEdge.syntax("se"), SidewalkVertex.syntax("sv"))

  def getSide(code: Int) = if (code == 0) NorthSide else SouthSide

  def getSideCode(side: Side) = side match {
    case NorthSide ⇒ 0
    case SouthSide ⇒ 1
  }

  def sidewalkEdge(e: SyntaxProvider[SidewalkEdge])(rs: WrappedResultSet): SidewalkEdge = sidewalkEdge(e.resultName)(rs)

  private def sidewalkEdge(e: ResultName[SidewalkEdge])(implicit rs: WrappedResultSet): SidewalkEdge = {
    SidewalkEdge(
      vertexStartId = rs.long(e.vertexStartId),
      vertexEndId = rs.long(e.vertexEndId),
      keyValue = rs.string(e.keyValue),
      id = rs.longOpt(e.id),
      side = getSide(rs.int(e.side)),
      streetEdgeBelongToId = rs.longOpt(e.streetEdgeBelongToId),
      isAccessible = rs.boolean(e.isAccessible))
  }

  def opt(e: SyntaxProvider[SidewalkEdge])(rs: WrappedResultSet): Option[SidewalkEdge] =
    rs.longOpt(e.resultName.id).map(_ ⇒ sidewalkEdge(e)(rs))

  def create(edge: SidewalkEdge)(implicit session: DBSession = SidewalkEdge.autoSession): Long = {
    withSQL {
      insert.into(SidewalkEdge).namedValues(
        SidewalkEdge.column.vertexStartId -> edge.vertexStartId,
        SidewalkEdge.column.vertexEndId -> edge.vertexEndId,
        SidewalkEdge.column.keyValue -> edge.keyValue,
        SidewalkEdge.column.side -> getSideCode(edge.side),
        SidewalkEdge.column.streetEdgeBelongToId -> edge.streetEdgeBelongToId.get,
        SidewalkEdge.column.isAccessible -> edge.isAccessible)
    }.updateAndReturnGeneratedKey.apply()
  }

  def save(edge: SidewalkEdge)(implicit session: DBSession = SidewalkEdge.autoSession): SidewalkEdge = {
    withSQL {
      update(SidewalkEdge).set(
        SidewalkEdge.column.vertexStartId -> edge.vertexStartId,
        SidewalkEdge.column.vertexEndId -> edge.vertexEndId,
        SidewalkEdge.column.keyValue -> edge.keyValue,
        SidewalkEdge.column.side -> getSideCode(edge.side),
        SidewalkEdge.column.streetEdgeBelongToId -> edge.streetEdgeBelongToId.get,
        SidewalkEdge.column.isAccessible -> edge.isAccessible).where.eq(SidewalkEdge.column.id, edge.id)
    }.update.apply()
    edge
  }

  def find(id: Long)(implicit session: DBSession = SidewalkEdge.autoSession): SidewalkEdge = withSQL {
    select
      .from(SidewalkEdge as se)
      .where.eq(se.id, id)
  }.map(sidewalkEdge(se)).single().apply().get

  def findNearestSidewalks(coordinate: Coordinate, radius: Double)(implicit session: DBSession = SidewalkEdge.autoSession): List[SidewalkEdge] = withSQL {
    select
      .from(SidewalkEdge as se)
      .leftJoin(SidewalkVertex as sv).on(se.vertexStartId, sv.id) // FIXME hacer join con vertices end, agregando un on en el left join
      .where.append(clauseNearestByDistance(coordinate, radius, sv, "coordinate"))
  }.map(sidewalkEdge(se)).list().apply()

  def deleteAll(implicit session: DBSession = SidewalkEdge.autoSession): Unit = withSQL {
    deleteFrom(SidewalkEdge)
  }.update.apply()

  def findSidewalkEdgesBySidewalkVertex(vertexId: Long)(implicit session: DBSession = SidewalkEdge.autoSession): List[SidewalkEdge] = DB readOnly { implicit session ⇒
    withSQL {
      select
        .from(SidewalkEdge as se)
        .where.eq(se.vertexStartId, vertexId).or.eq(se.vertexEndId, vertexId)
    }.map(sidewalkEdge(se)).list.apply()
  }

}

object SidewalkEdgeRepository extends SidewalkEdgeRepository