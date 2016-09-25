package mapdomain.street

import mapdomain.graph.Coordinate
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport
import sqls.count

trait StreetRepositorySupport {
  val streetVertexRepository = StreetVertexRepository
  val streetEdgeRepository = StreetEdgeRepository
  val streetInfoRepository = StreetInfoRepository
}

trait StreetVertexRepository extends SpatialSQLSupport {

  val v = StreetVertex.syntax("v")
  val edge1 = StreetEdge.syntax("edge1")
  val edge2 = StreetEdge.syntax("edge2")
  val neighbour = StreetVertex.syntax("neighbour")

  protected def streetVertex(v: SyntaxProvider[StreetVertex], withEdges: Boolean = true)(rs: WrappedResultSet): StreetVertex = {
    val vertex: StreetVertex = StreetVertex(rs.long(v.resultName.id), Nil, coordinateFromResultSet(rs, v.tableAliasName))
    if (withEdges) {
      val streetEdges: List[StreetEdge] = StreetEdgeRepository.findStreetEdgesByStreetVertex(vertex.id)
      vertex.copy(edges = streetEdges)
    } else vertex
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
  }.map(streetVertex(v)).single().apply()

  // FIXME adaptar a streaming @see https://github.com/tkawachi/scalikejdbc-stream
  def findAll(implicit session: DBSession = StreetVertex.autoSession): List[StreetVertex] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
  }.map(streetVertex(v)).list().apply()

  def findAllWithoutOf(ids: List[Long])(implicit session: DBSession = StreetVertex.autoSession): List[StreetVertex] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
      .where.notIn(v.id, ids)
  }.map(streetVertex(v)).list().apply()

  def findNearest(coordinate: Coordinate)(implicit session: DBSession = StreetVertex.autoSession): Option[StreetVertex] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
      .append(orderByDistance(coordinate, v, "coordinate")) // FIXME limitar con un where
      .limit(1)
  }.map(streetVertex(v)).single().apply()

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
    """.map(streetVertex(neighbour))
      .list.apply()
  }

  def totalVertices(implicit session: DBSession = StreetVertex.autoSession): Long = withSQL {
    select(count)
      .from(StreetVertex as v)
  }.map(_.long(1)).single().apply().get

}

object StreetVertexRepository extends StreetVertexRepository

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

trait StreetInfoRepository extends SpatialSQLSupport {

  val (si, se) = (StreetInfo.syntax("si"), StreetEdge.syntax("se"))

  def streetInfo(si: SyntaxProvider[StreetInfo])(rs: WrappedResultSet): StreetInfo = streetInfo(si.resultName)(rs)

  private def streetInfo(si: ResultName[StreetInfo])(implicit rs: WrappedResultSet): StreetInfo = {
    new StreetInfo(
      Some(rs.long(si.id)),
      rs.stringOpt(si.address),
      rs.long(si.wayId))
  }

  def find(id: Long)(implicit session: DBSession = StreetInfo.autoSession): StreetInfo = withSQL {
    select.
      from(StreetInfo as si)
      .where.eq(si.id, id)
  }.map(streetInfo(si)).single().apply().get

  def findByStreetEdge(streetEdgeId: Long)(implicit session: DBSession = StreetInfo.autoSession): StreetInfo = withSQL {
    select
      .all(si)
      .from(StreetEdge as se)
      .innerJoin(StreetInfo as si).on(se.streetInfoId, si.id)
      .where.eq(se.id, streetEdgeId)
  }.map(streetInfo(si)).single().apply().get

  def create(streetInfo: StreetInfo)(implicit session: DBSession = StreetInfo.autoSession): Long = {
    withSQL {
      insert.into(StreetInfo).namedValues(
        StreetInfo.column.address -> streetInfo.address,
        StreetInfo.column.wayId -> streetInfo.wayId)
    }.updateAndReturnGeneratedKey.apply()
  }

  def deleteAll(implicit session: DBSession = StreetInfo.autoSession): Unit = withSQL {
    deleteFrom(StreetInfo)
  }.update.apply()

}
object StreetInfoRepository extends StreetInfoRepository
