package mapdomain.sidewalk

import base.{ FailureReporterSupport, LazyLoggerSupport }
import mapdomain.graph._
import mapdomain.math.Line
import mapdomain.sidewalk.SidewalkEdge.Side
import mapdomain.utils.GraphUtils

class PedestrianEdge(override val vertexStart: Long, override val vertexEnd: Long, key: String, override val distance: Double = 1) extends GeoEdge(vertexStart, vertexEnd, distance) {
  def from(implicit graphContainer: GraphContainer[SidewalkVertex]): Option[SidewalkVertex] = graphContainer.findVertex(vertexStart)
  def to(implicit graphContainer: GraphContainer[SidewalkVertex]): Option[SidewalkVertex] = graphContainer.findVertex(vertexEnd)
}

case class SidewalkEdge(override val vertexStart: Long, override val vertexEnd: Long, key: String,
  streetEdgeBelongTo: GeoEdge, side: Side) extends PedestrianEdge(vertexStart, vertexEnd, key)

object SidewalkEdge extends FailureReporterSupport with LazyLoggerSupport {

  def sideByEdges(streetLine: Line, sidewalkLine: Line): Side = withFailureLogging({
    if (Line.compareParallelsByAltitude(streetLine, sidewalkLine) == 1) SouthSide
    else NorthSide
  }, (exc: Throwable) ⇒ logger.error(s"Failed trying to calculate the side of a sidewalk from an edge.", exc))

  def generateKey[E <: GeoEdge](edgeBelongTo: E, side: Side): String = {
    val vertexStartId: Long = edgeBelongTo.vertexStart
    val vertexEndId: Long = edgeBelongTo.vertexEnd
    val idPart = if (vertexStartId > vertexEndId) s"$vertexEndId-$vertexStartId" else s"$vertexStartId-$vertexEndId"
    s"$idPart-$side"
  }

  trait Side
  case object NorthSide extends Side
  case object SouthSide extends Side
}

case class StreetCrossingEdge(override val vertexStart: Long, override val vertexEnd: Long, key: String) extends PedestrianEdge(vertexStart, vertexEnd, key)

case class SidewalkVertex(override val id: Long, override val coordinate: Coordinate, sidewalkEdges: List[SidewalkEdge],
    streetCrossingEdges: List[StreetCrossingEdge], streetVertexBelongTo: GeoVertex) extends GeoVertex(id, sidewalkEdges ++ streetCrossingEdges, coordinate) {

  lazy val neighbourIds: List[Long] = edges.map(edge ⇒ if (edge.vertexStart == id) edge.vertexEnd else edge.vertexStart)

  override def neighbours[V <: Vertex](graph: GraphContainer[V]): List[V] = neighbourIds.flatMap(id ⇒ graph.findVertex(id) toList)
}

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

