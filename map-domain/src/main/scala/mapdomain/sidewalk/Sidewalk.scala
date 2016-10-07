package mapdomain.sidewalk

import base.{ FailureReporterSupport, LazyLoggerSupport }
import mapdomain.graph._
import mapdomain.math.Line
import scalikejdbc._

trait PedestrianEdge extends GeoEdge with BaseEntity {
  val distance: Double = 1
  def from(implicit graphContainer: SidewalkGraphContainer): Option[SidewalkVertex] = graphContainer.findVertex(vertexStartId)
  def to(implicit graphContainer: SidewalkGraphContainer): Option[SidewalkVertex] = graphContainer.findVertex(vertexEndId)
}

trait Side
case object NorthSide extends Side
case object SouthSide extends Side

case class SidewalkEdge(vertexStartId: Long, vertexEndId: Long, keyValue: String, side: Side, streetEdgeBelongToId: Option[Long],
                        override val id: Option[Long] = None, isAccessible: Boolean = true) extends PedestrianEdge

object SidewalkEdge extends FailureReporterSupport with LazyLoggerSupport with SQLSyntaxSupport[SidewalkEdge] {

  override val tableName = "sidewalk_edge"

  override val useSnakeCaseColumnName = false

  override val columns = Seq("id", "vertexStartId", "vertexEndId", "keyValue", "side", "streetEdgeBelongToId", "isAccessible")

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

}

case class StreetCrossingEdge(override val vertexStartId: Long, override val vertexEndId: Long,
  keyValue: String, override val id: Option[Long] = None, rampStartId: Option[Long] = None,
  rampEndId: Option[Long] = None) extends PedestrianEdge

object StreetCrossingEdge extends SQLSyntaxSupport[StreetCrossingEdge] {

  override val tableName = "street_crossing_edge"

  override val useSnakeCaseColumnName = false

}

trait SidewalkVertex extends GeoVertex[PedestrianEdge] {

  val coordinate: Coordinate
  val sidewalkEdges: List[SidewalkEdge]
  val streetCrossingEdges: List[StreetCrossingEdge]
  val streetVertexBelongToId: Long

  val edges: List[PedestrianEdge] = sidewalkEdges ++ streetCrossingEdges

  override def getEdgesFor(vertexId: Long): Option[PedestrianEdge] = edges.find(edge ⇒ edge.vertexEndId == vertexId || edge.vertexStartId == vertexId)

  def copy(id: Long = id, coordinate: Coordinate = coordinate, sidewalkEdges: List[SidewalkEdge] = sidewalkEdges,
           streetCrossingEdges: List[StreetCrossingEdge] = streetCrossingEdges, streetVertexBelongToId: Long = streetVertexBelongToId): SidewalkVertex = {
    SidewalkVertex(id, coordinate, sidewalkEdges, streetCrossingEdges, streetVertexBelongToId)
  }
}

object SidewalkVertex extends SQLSyntaxSupport[SidewalkVertex] {
  override val tableName = "sidewalk_vertex"
  override val useSnakeCaseColumnName = false

  def apply(id: Long, coordinate: Coordinate, sidewalkEdges: List[SidewalkEdge],
            streetCrossingEdges: List[StreetCrossingEdge], streetVertexBelongToId: Long): SidewalkVertex = {
    new SidewalkVertexImpl(id, coordinate, sidewalkEdges, streetCrossingEdges, streetVertexBelongToId)
  }
}

class SidewalkVertexImpl(val id: Long, val coordinate: Coordinate, val sidewalkEdges: List[SidewalkEdge],
                              val streetCrossingEdges: List[StreetCrossingEdge], val streetVertexBelongToId: Long) extends SidewalkVertex
