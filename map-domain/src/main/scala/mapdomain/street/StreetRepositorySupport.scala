package mapdomain.street

import mapdomain.graph.Coordinate
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport

trait StreetRepositorySupport {
  val streetVertexRepository = StreetVertexRepository
  val streetEdgeRepository = StreetEdgeRepository
}

trait StreetVertexRepository extends SpatialSQLSupport {

  val v = StreetVertex.syntax("v")
  val edge1 = StreetEdge.syntax("edge1")
  val edge2 = StreetEdge.syntax("edge2")
  val neighbour = StreetVertex.syntax("neighbour")

  def streetVertexOnly(v: SyntaxProvider[StreetVertex])(rs: WrappedResultSet): StreetVertex = {
    StreetVertex(rs.long(v.resultName.id), Nil, coordinateFromResultSet(rs, v.tableAliasName))
  }

  def create(streetVertex: StreetVertex)(implicit session: DBSession = StreetVertex.autoSession): StreetVertex = {
    withSQL {
      insert.into(StreetVertex).namedValues(
        StreetVertex.column.id -> streetVertex.id,
        StreetVertex.column.coordinate -> positionToSQL(streetVertex.coordinate))
    }.update().apply()

    streetVertex
  }

  def createInBulk(streetVertices: List[StreetVertex])(implicit session: DBSession = StreetVertex.autoSession): List[StreetVertex] = {
    streetVertices foreach create
    streetVertices
  }

  def find(id: Long)(implicit session: DBSession = StreetVertex.autoSession): Option[StreetVertex] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
      .where.eq(v.id, id)
  }.map(streetVertexOnly(v)).single().apply()

  def findNearest(coordinate: Coordinate)(implicit session: DBSession = StreetVertex.autoSession): Option[StreetVertex] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
      .append(orderBy(coordinate, v, "coordinate")) // FIXME limitar con un where
      .limit(1)
  }.map(streetVertexOnly(v)).single().apply()

  def deleteAll(implicit session: DBSession = StreetVertex.autoSession): Unit = withSQL {
    deleteFrom(StreetVertex)
  }.update.apply()

  def findNeighbours(vertexId: Long)(implicit session: DBSession = StreetVertex.autoSession): List[StreetVertex] = DB readOnly { implicit session ⇒
    sql"""
       select
        distinct ${neighbour.result.*} ${selectLatitudeAndLongitude(neighbour)}
       from
        ${StreetVertex.as(v)}
        left join ${StreetEdge.as(edge1)} on ${edge1.vertexStartId} = ${v.id}
        left join ${StreetEdge.as(edge2)} on ${edge2.vertexEndId} = ${v.id}
        left join ${StreetVertex.as(neighbour)} on ${neighbour.id} IN (${edge1.vertexEndId}, ${edge2.vertexStartId})
       where
        ${v.id} = ${vertexId}
    """.map(streetVertexOnly(neighbour))
      .list.apply()
  }

}

object StreetVertexRepository extends StreetVertexRepository

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
