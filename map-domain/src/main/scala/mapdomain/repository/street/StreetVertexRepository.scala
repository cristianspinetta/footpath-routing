package mapdomain.repository.street

import base.{LazyLoggerSupport, MeterSupport}
import mapdomain.graph.Coordinate
import mapdomain.street.{StreetEdge, StreetVertex}
import scalikejdbc._
import sql.SpatialSQLSupport
import sqls.count

trait StreetVertexRepository extends SpatialSQLSupport with MeterSupport with LazyLoggerSupport {

  val v = StreetVertex.syntax("v")
  val edge1 = StreetEdge.syntax("edge1")
  val edge2 = StreetEdge.syntax("edge2")
  val neighbour = StreetVertex.syntax("neighbour")

  protected def streetVertex(v: SyntaxProvider[StreetVertex[StreetEdge]], withEdges: Boolean = true)(rs: WrappedResultSet): StreetVertex[StreetEdge] = {
    val vertex: StreetVertex[StreetEdge] = StreetVertex(rs.long(v.resultName.id), Nil, coordinateFromResultSet(rs, v.tableAliasName))
    if (withEdges) {
      val streetEdges: List[StreetEdge] = StreetEdgeRepository.findStreetEdgesByStreetVertex(vertex.id)
      vertex.copy(edges = streetEdges)
    } else vertex
  }

  def create[V <: StreetVertex[_]](streetVertex: V)(implicit session: DBSession = StreetVertex.autoSession): V = {
    withSQL {
      insert.into(StreetVertex).namedValues(
        StreetVertex.column.id -> streetVertex.id,
        StreetVertex.column.coordinate -> positionToSQL(streetVertex.coordinate))
    }.update().apply()

    streetVertex
  }

  def createInBulk[V <: StreetVertex[_]](streetVertices: List[V])(implicit session: DBSession = StreetVertex.autoSession): List[V] = {
    streetVertices foreach create
    streetVertices
  }

  def find(id: Long)(implicit session: DBSession = StreetVertex.autoSession): Option[StreetVertex[StreetEdge]] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
      .where.eq(v.id, id)
  }.map(streetVertex(v)).single().apply()

  // FIXME adaptar a streaming @see https://github.com/tkawachi/scalikejdbc-stream
  def findAll(implicit session: DBSession = StreetVertex.autoSession): List[StreetVertex[StreetEdge]] = withTimeLogging(
    withSQL {
      select
        .all(v)
        .append(selectLatitudeAndLongitude(v))
        .from(StreetVertex as v)
    }.map(streetVertex(v)).list().apply(),
    (time: Long) => logger.info(s"Populate Street Vertex from DB took $time ms."))

  def findAllWithoutOf(ids: List[Long])(implicit session: DBSession = StreetVertex.autoSession): List[StreetVertex[StreetEdge]] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
      .where.notIn(v.id, ids)
  }.map(streetVertex(v)).list().apply()

  def findNearest(coordinate: Coordinate)(implicit session: DBSession = StreetVertex.autoSession): Option[StreetVertex[StreetEdge]] = withSQL {
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

  def findNeighbours(vertexId: Long)(implicit session: DBSession = StreetVertex.autoSession): List[StreetVertex[StreetEdge]] = DB readOnly { implicit session ⇒
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