package mapdomain.sidewalk

import base.{ FailureReporterSupport, LazyLoggerSupport }
import mapdomain.graph._
import mapdomain.math.Line
import mapdomain.sidewalk.SidewalkEdge.Side
import mapdomain.street.OsmVertexRepository
import mapdomain.street.OsmVertex
import mapdomain.utils.GraphUtils
import scalikejdbc._
import sql.SpatialSQLSupport

class PedestrianEdge(override val vertexStartId: Long, override val vertexEndId: Long, key: String, override val distance: Double = 1) extends GeoEdge(vertexStartId, vertexEndId, distance) {
  def from(implicit graphContainer: GraphContainer[SidewalkVertex]): Option[SidewalkVertex] = graphContainer.findVertex(vertexStartId)
  def to(implicit graphContainer: GraphContainer[SidewalkVertex]): Option[SidewalkVertex] = graphContainer.findVertex(vertexEndId)
}

case class SidewalkEdge(override val vertexStartId: Long, override val vertexEndId: Long, keyValue: String,
  streetEdgeBelongTo: GeoEdge, side: Side, val id: Option[Long] = None) extends PedestrianEdge(vertexStartId, vertexEndId, keyValue)

object SidewalkEdge extends FailureReporterSupport with LazyLoggerSupport {

  def sideByEdges(streetLine: Line, sidewalkLine: Line): Side = withFailureLogging({
    if (Line.compareParallelsByAltitude(streetLine, sidewalkLine) == 1) SouthSide
    else NorthSide
  }, (exc: Throwable) ⇒ logger.error(s"Failed trying to calculate the side of a sidewalk from an edge.", exc))

  def generateKey[E <: GeoEdge](edgeBelongTo: E, side: Side): String = {
    val vertexStartId: Long = edgeBelongTo.vertexStartId
    val vertexEndId: Long = edgeBelongTo.vertexEndId
    val idPart = if (vertexStartId > vertexEndId) s"$vertexEndId-$vertexStartId" else s"$vertexStartId-$vertexEndId"
    s"$idPart-$side"
  }

  trait Side
  case object NorthSide extends Side
  case object SouthSide extends Side
}

case class StreetCrossingEdge(override val vertexStartId: Long, override val vertexEndId: Long, keyValue: String, val id: Option[Long] = None) extends PedestrianEdge(vertexStartId, vertexEndId, keyValue)

object StreetCrossingEdge extends SQLSyntaxSupport[StreetCrossingEdge] {

  override val tableName = "StreetCrossingEdge"

  override val useSnakeCaseColumnName = false

}

trait StreetCrossingEdgeRepository {

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

  def deleteAll(implicit session: DBSession = StreetCrossingEdge.autoSession): Unit = withSQL {
    deleteFrom(StreetCrossingEdge)
  }.update.apply()

  def findCrossingEdgesBySidewalkVertex(vertexId: Long)(implicit session: DBSession = StreetCrossingEdge.autoSession): List[StreetCrossingEdge] = DB readOnly { implicit session ⇒
    sql"""
       select
        ${sce.result.*}
       from
        ${SidewalkVertex.as(sv)}
        left join ${StreetCrossingEdge.as(sce)} on ${sce.vertexStartId} = ${sv.id}
       where
        ${sv.id} = ${vertexId}
    """.map(streetCrossingEdge(sce))
      .list.apply()
  }

}

object StreetCrossingEdgeRepository extends StreetCrossingEdgeRepository

case class SidewalkVertex(override val id: Long, override val coordinate: Coordinate, sidewalkEdges: List[SidewalkEdge],
    streetCrossingEdges: List[StreetCrossingEdge], streetVertexBelongTo: GeoVertex, streetVertexBelongToId: Option[Long] = None) extends GeoVertex(id, sidewalkEdges ++ streetCrossingEdges, coordinate) {

  lazy val neighbourIds: List[Long] = edges.map(edge ⇒ if (edge.vertexStartId == id) edge.vertexEndId else edge.vertexStartId)

  override def getEdgesFor(vertexId: Long): Option[Edge] = edges.find(edge ⇒ edge.vertexEndId == vertexId || edge.vertexStartId == vertexId)

  override def neighbours[V <: Vertex](graph: GraphContainer[V]): List[V] = neighbourIds.flatMap(id ⇒ graph.findVertex(id) toList)
}

object SidewalkVertex extends SQLSyntaxSupport[SidewalkVertex] {

  override val tableName = "SidewalkVertex"

  override val useSnakeCaseColumnName = false

}

trait SidewalkVertexRepository extends SpatialSQLSupport {

  val s = SidewalkVertex.syntax("s")
  private val v = OsmVertexRepository.v

  def sidewalkVertex(s: SyntaxProvider[SidewalkVertex])(rs: WrappedResultSet): SidewalkVertex = sidewalkVertex(s.resultName, s.tableAliasName)(rs)

  private def sidewalkVertex(v: ResultName[SidewalkVertex], tableAlias: String)(rs: WrappedResultSet): SidewalkVertex =
    SidewalkVertex(rs.long(v.id), coordinateFromResultSet(rs, tableAlias), Nil, Nil, null, Some(rs.long(v.streetVertexBelongToId)))

  private def sidewalkVertex(s: SyntaxProvider[SidewalkVertex], v: SyntaxProvider[OsmVertex])(rs: WrappedResultSet): SidewalkVertex = {
    sidewalkVertex(s)(rs).copy(streetVertexBelongTo = OsmVertexRepository.osmVertexOnly(v)(rs))
  }

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
      .all(s, v)
      .append(selectLatitudeAndLongitude(s))
      .append(selectLatitudeAndLongitude(v))
      .from(SidewalkVertex as s)
      .leftJoin(OsmVertex as v)
      .on(s.streetVertexBelongToId, v.id)
      .where.eq(s.id, id)
  }.map(sidewalkVertex(s, v)).single().apply()

  def deleteAll(implicit session: DBSession = SidewalkVertex.autoSession): Unit = withSQL {
    deleteFrom(SidewalkVertex)
  }.update.apply()

}

object SidewalkVertexRepository extends SidewalkVertexRepository

case class SidewalkGraphContainer(override val vertices: List[SidewalkVertex]) extends GraphContainer(vertices) {

  override def purge: SidewalkGraphContainer = GraphUtils.getConnectedComponent(this, SidewalkGraphContainer.apply)

  lazy val sidewalkEdges: List[SidewalkEdge] = {
    (for (vertex ← vertices; edge ← vertex.sidewalkEdges) yield (edge.keyValue, edge))
      .groupBy { case (key, _) ⇒ key }
      .map { case (_, (_, edge) :: edgesTail) ⇒ edge }
      .toList
  }
  lazy val streetCrossingEdges: List[StreetCrossingEdge] = {
    (for (vertex ← vertices; edge ← vertex.streetCrossingEdges) yield (edge.keyValue, edge))
      .groupBy { case (key, _) ⇒ key }
      .map { case (_, (_, edge) :: edgesTail) ⇒ edge }
      .toList
  }
}

