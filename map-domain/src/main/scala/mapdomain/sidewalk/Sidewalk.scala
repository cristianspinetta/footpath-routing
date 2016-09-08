package mapdomain.sidewalk

import base.{FailureReporterSupport, LazyLoggerSupport}
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

case class SidewalkEdge(override val vertexStartId: Long, override val vertexEndId: Long, key: String,
  streetEdgeBelongTo: GeoEdge, side: Side) extends PedestrianEdge(vertexStartId, vertexEndId, key)

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

case class StreetCrossingEdge(override val vertexStartId: Long, override val vertexEndId: Long, key: String) extends PedestrianEdge(vertexStartId, vertexEndId, key)

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
        SidewalkVertex.column.streetVertexBelongToId -> sidewalkVertex.streetVertexBelongToId
      )
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
    (for (vertex ← vertices; edge ← vertex.sidewalkEdges) yield (edge.key, edge))
      .groupBy { case (key, _) ⇒ key }
      .map { case (_, (_, edge) :: edgesTail) ⇒ edge }
      .toList
  }
  lazy val streetCrossingEdges: List[StreetCrossingEdge] = {
    (for (vertex ← vertices; edge ← vertex.streetCrossingEdges) yield (edge.key, edge))
      .groupBy { case (key, _) ⇒ key }
      .map { case (_, (_, edge) :: edgesTail) ⇒ edge }
      .toList
  }
}

