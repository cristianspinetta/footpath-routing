package mapdomain.sidewalk

import mapdomain.graph._
import mapdomain.utils.GraphUtils

trait SidewalkGraphContainer[V <: SidewalkVertex] extends GeoGraphContainer[V]

case class LazySidewalkGraphContainer() extends LazyGeoGraphContainer[SidewalkVertex] with SidewalkGraphContainer[SidewalkVertex] with SidewalkRepositorySupport {
  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[SidewalkVertex] = SidewalkVertexRepository.find(id)

  override def findNearest(coordinate: Coordinate): Option[SidewalkVertex] = sidewalkVertexRepository.findNearest(coordinate)

  def findNearestVertex(coordinate: Coordinate): Option[SidewalkVertex] = findNearest(coordinate)

  override def neighbours(vertex: SidewalkVertex): Seq[SidewalkVertex] = ???
}

case class EagerSidewalkGraphContainer(override val vertices: List[SidewalkVertex]) extends EagerGeoGraphContainer(vertices) {

  /**
   * Create a new EagerSidewalkGraphContainer with maximal connected subgraph that this graph contains
   * @return The connected graph
   */
  def purgeSidewalks: EagerSidewalkGraphContainer = GraphUtils.getConnectedComponent(this, EagerSidewalkGraphContainer.apply)

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
