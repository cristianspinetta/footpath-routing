package mapdomain.sidewalk

import mapdomain.graph.{ BoundedGeoLocation, Coordinate }
import scalikejdbc.{ DB, DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport

trait SidewalkRepositorySupport {
  protected val sidewalkVertexRepository = SidewalkVertexRepository
  protected val sidewalkEdgeRepository = SidewalkEdgeRepository
  protected val streetCrossingEdgeRepository = StreetCrossingEdgeRepository
}

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
      streetEdgeBelongToId = rs.longOpt(e.streetEdgeBelongToId))
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
        SidewalkEdge.column.streetEdgeBelongToId -> edge.streetEdgeBelongToId.get)
    }.updateAndReturnGeneratedKey.apply()
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
        .where.eq(se.vertexStartId, vertexId)
    }.map(sidewalkEdge(se)).list.apply()
  }

}

object SidewalkEdgeRepository extends SidewalkEdgeRepository

trait StreetCrossingEdgeRepository extends SpatialSQLSupport {

  val (sce, sv) = (StreetCrossingEdge.syntax("sce"), SidewalkVertex.syntax("sv"))

  def streetCrossingEdge(e: SyntaxProvider[StreetCrossingEdge])(rs: WrappedResultSet): StreetCrossingEdge = streetCrossingEdge(e.resultName)(rs)

  private def streetCrossingEdge(e: ResultName[StreetCrossingEdge])(implicit rs: WrappedResultSet): StreetCrossingEdge = {
    new StreetCrossingEdge(
      vertexStartId = rs.long(e.vertexStartId),
      vertexEndId = rs.long(e.vertexEndId),
      keyValue = rs.string(e.keyValue),
      id = rs.longOpt(e.id))
  }

  def opt(e: SyntaxProvider[StreetCrossingEdge])(rs: WrappedResultSet): Option[StreetCrossingEdge] =
    rs.longOpt(e.resultName.id).map(_ ⇒ streetCrossingEdge(e)(rs))

  def create(edge: StreetCrossingEdge)(implicit session: DBSession = StreetCrossingEdge.autoSession): Long = {
    withSQL {
      insert.into(StreetCrossingEdge).namedValues(
        StreetCrossingEdge.column.vertexStartId -> edge.vertexStartId,
        StreetCrossingEdge.column.vertexEndId -> edge.vertexEndId,
        StreetCrossingEdge.column.keyValue -> edge.keyValue)
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
        .where.eq(sce.vertexStartId, vertexId)
    }.map(streetCrossingEdge(sce)).list.apply()
  }

}

object StreetCrossingEdgeRepository extends StreetCrossingEdgeRepository

trait SidewalkVertexRepository extends SpatialSQLSupport {

  val s = SidewalkVertex.syntax("s")
  val sidewalkEdge1 = SidewalkEdge.syntax("sidewalkEdge1")
  val sidewalkEdge2 = SidewalkEdge.syntax("sidewalkEdge2")
  val crossingEdge1 = StreetCrossingEdge.syntax("crossingEdge1")
  val crossingEdge2 = StreetCrossingEdge.syntax("crossingEdge2")
  val neighbour = SidewalkVertex.syntax("neighbour")

  def sidewalkVertex(s: SyntaxProvider[SidewalkVertex], withEdges: Boolean = true)(rs: WrappedResultSet): SidewalkVertex = {
    val vertex: SidewalkVertex = sidewalkVertex(s.resultName, s.tableAliasName)(rs)
    if (withEdges) {
      val sidewalkEdges: List[SidewalkEdge] = SidewalkEdgeRepository.findSidewalkEdgesBySidewalkVertex(vertex.id)
      val streetCrossingEdges: List[StreetCrossingEdge] = StreetCrossingEdgeRepository.findCrossingEdgesBySidewalkVertex(vertex.id)
      vertex.copy(sidewalkEdges = sidewalkEdges, streetCrossingEdges = streetCrossingEdges)
    } else vertex
  }

  private def sidewalkVertex(v: ResultName[SidewalkVertex], tableAlias: String)(rs: WrappedResultSet): SidewalkVertex =
    SidewalkVertex(rs.long(v.id), coordinateFromResultSet(rs, tableAlias), Nil, Nil, rs.long(v.streetVertexBelongToId))

  def create(sidewalkVertex: SidewalkVertex)(implicit session: DBSession = SidewalkVertex.autoSession): SidewalkVertex = {
    withSQL {
      insert.into(SidewalkVertex).namedValues(
        SidewalkVertex.column.id -> sidewalkVertex.id,
        SidewalkVertex.column.coordinate -> positionToSQL(sidewalkVertex.coordinate),
        SidewalkVertex.column.streetVertexBelongToId -> sidewalkVertex.streetVertexBelongToId)
    }.update().apply()

    sidewalkVertex
  }

  def find(id: Long)(implicit session: DBSession = SidewalkVertex.autoSession): Option[SidewalkVertex] = withSQL {
    select
      .all(s)
      .append(selectLatitudeAndLongitude(s))
      .from(SidewalkVertex as s)
      .where.eq(s.id, id)
  }.map(sidewalkVertex(s)).single().apply()

  // FIXME adaptar a streaming @see https://github.com/tkawachi/scalikejdbc-stream
  def findAll(implicit session: DBSession = SidewalkVertex.autoSession): List[SidewalkVertex] = withSQL {
    select
      .all(s)
      .append(selectLatitudeAndLongitude(s))
      .from(SidewalkVertex as s)
  }.map(sidewalkVertex(s)).list().apply()

  def findNearest(coordinate: Coordinate)(implicit session: DBSession = SidewalkVertex.autoSession): Option[SidewalkVertex] = withSQL {
    select
      .all(s)
      .append(selectLatitudeAndLongitude(s))
      .from(SidewalkVertex as s)
      .append(orderByDistance(coordinate, s, "coordinate")) // FIXME limitar con un where
      .limit(1)
  }.map(sidewalkVertex(s)).single().apply()

  def findNeighbours(vertexId: Long)(implicit session: DBSession = SidewalkVertex.autoSession): List[SidewalkVertex] = DB readOnly { implicit session ⇒
    sql"""
       select
        distinct ${neighbour.result.*} ${selectLatitudeAndLongitude(neighbour)}
       from
        ${SidewalkVertex.as(s)}
        left join ${SidewalkEdge.as(sidewalkEdge1)} on ${sidewalkEdge1.vertexStartId} = ${s.id}
        left join ${SidewalkEdge.as(sidewalkEdge2)} on ${sidewalkEdge2.vertexEndId} = ${s.id}
        left join ${StreetCrossingEdge.as(crossingEdge1)} on ${crossingEdge1.vertexStartId} = ${s.id}
        left join ${StreetCrossingEdge.as(crossingEdge2)} on ${crossingEdge2.vertexEndId} = ${s.id}
        left join ${SidewalkVertex.as(neighbour)} on ${neighbour.id} IN (sidewalkEdge1.vertexEndId, sidewalkEdge2.vertexStartId, crossingEdge1.vertexEndId, crossingEdge2.vertexStartId)
       where
        ${s.id} = ${vertexId}
    """.map(sidewalkVertex(neighbour))
      .list.apply()
  }

  def deleteAll(implicit session: DBSession = SidewalkVertex.autoSession): Unit = withSQL {
    deleteFrom(SidewalkVertex)
  }.update.apply()

}

object SidewalkVertexRepository extends SidewalkVertexRepository
