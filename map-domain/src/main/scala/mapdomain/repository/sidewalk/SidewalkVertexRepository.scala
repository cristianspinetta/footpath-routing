package mapdomain.repository.sidewalk

import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph.Coordinate
import mapdomain.sidewalk._
import scalikejdbc._
import scalikejdbc.interpolation.SQLSyntax._
import sql.SpatialSQLSupport

trait SidewalkVertexRepository extends SpatialSQLSupport with MeterSupport with LazyLoggerSupport {

  val s = SidewalkVertex.syntax("s")
  val sidewalkEdge1 = SidewalkEdge.syntax("sidewalkEdge1")
  val sidewalkEdge2 = SidewalkEdge.syntax("sidewalkEdge2")
  val crossingEdge1 = StreetCrossingEdge.syntax("crossingEdge1")
  val crossingEdge2 = StreetCrossingEdge.syntax("crossingEdge2")

  val neighbour = SidewalkVertex.syntax("neighbour")
  val sidewalkEdge = SidewalkEdge.syntax("sidewalkEdge")
  val crossingEdge = StreetCrossingEdge.syntax("crossingEdge")
  val ramp1 = Ramp.syntax("ramp1")
  val ramp2 = Ramp.syntax("ramp2")

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
  def findAll(implicit session: DBSession = SidewalkVertex.autoSession): List[SidewalkVertex] = withTimeLogging(
    withSQL {
      select
        .all(s)
        .append(selectLatitudeAndLongitude(s))
        .from(SidewalkVertex as s)
    }.map(sidewalkVertex(s)).list().apply(),
    (time: Long) ⇒ logger.info(s"Getting of all Sidewalk Vertex finished in $time ms."))

  def findAllWithoutOf(ids: List[Long])(implicit session: DBSession = SidewalkVertex.autoSession): List[SidewalkVertex] = withSQL {
    select
      .all(s)
      .append(selectLatitudeAndLongitude(s))
      .from(SidewalkVertex as s)
      .where.notIn(s.id, ids)
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
       select ${neighbour.result.*} ${selectLatitudeAndLongitude(neighbour)}
       from ${SidewalkEdge.as(sidewalkEdge)}
       	join ${SidewalkVertex.as(neighbour)} on ${neighbour.id} = ${sidewalkEdge.vertexEndId}
       where ${sidewalkEdge.vertexStartId} = ${vertexId} and ${sidewalkEdge.isAccessible}

       union

       select ${neighbour.result.*} ${selectLatitudeAndLongitude(neighbour)}
       from ${SidewalkEdge.as(sidewalkEdge)}
        join ${SidewalkVertex.as(neighbour)} on ${neighbour.id} = ${sidewalkEdge.vertexStartId}
       where ${sidewalkEdge.vertexEndId} = ${vertexId} and ${sidewalkEdge.isAccessible}

       union

       select ${neighbour.result.*} ${selectLatitudeAndLongitude(neighbour)}
       from ${StreetCrossingEdge.as(crossingEdge)}
       	join ${SidewalkVertex.as(neighbour)} on ${neighbour.id} = ${crossingEdge.vertexEndId}
       	join ${Ramp.as(ramp1)} on ${ramp1.id} = ${crossingEdge.rampEndId}
        join ${Ramp.as(ramp2)} on ${ramp2.id} = ${crossingEdge.rampStartId}
       where ${crossingEdge.vertexStartId} = ${vertexId} and ${ramp1.isAccessible} and ${ramp2.isAccessible}

       union

       select ${neighbour.result.*} ${selectLatitudeAndLongitude(neighbour)}
       from ${StreetCrossingEdge.as(crossingEdge)}
        join ${SidewalkVertex.as(neighbour)} on ${neighbour.id} = ${crossingEdge.vertexStartId}
        join ${Ramp.as(ramp1)} on ${ramp1.id} = ${crossingEdge.rampStartId}
        join ${Ramp.as(ramp2)} on ${ramp2.id} = ${crossingEdge.rampEndId}
       where ${crossingEdge.vertexEndId} = ${vertexId} and ${ramp1.isAccessible} and ${ramp2.isAccessible}
    """.map(sidewalkVertex(neighbour))
      .list.apply()
  }

  def totalVertices(implicit session: DBSession = SidewalkVertex.autoSession): Long = withSQL {
    select(count)
      .from(SidewalkVertex as s)
  }.map(_.long(1)).single().apply().get

  def deleteAll(implicit session: DBSession = SidewalkVertex.autoSession): Unit = withSQL {
    deleteFrom(SidewalkVertex)
  }.update.apply()

}

object SidewalkVertexRepository extends SidewalkVertexRepository