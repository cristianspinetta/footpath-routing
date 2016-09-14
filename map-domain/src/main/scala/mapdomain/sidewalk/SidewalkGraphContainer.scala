package mapdomain.sidewalk

import base.LazyLoggerSupport
import mapdomain.graph._
import mapdomain.utils.GraphUtils

trait SidewalkGraphContainer extends GeoGraphContainer[SidewalkVertex] {
  def findNearestSidewalks(coordinate: Coordinate, radius: Double): List[SidewalkEdge]
  def findNearestStreetCrossing(coordinate: Coordinate, radius: Double): List[StreetCrossingEdge]
}

case class LazySidewalkGraphContainer() extends LazyGeoGraphContainer[SidewalkVertex] with SidewalkGraphContainer with SidewalkRepositorySupport {
  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[SidewalkVertex] = SidewalkVertexRepository.find(id)

  override def findNearest(coordinate: Coordinate): Option[SidewalkVertex] = sidewalkVertexRepository.findNearest(coordinate)

  def findNearestVertex(coordinate: Coordinate): Option[SidewalkVertex] = findNearest(coordinate)

  override def neighbours(vertex: SidewalkVertex): List[SidewalkVertex] = sidewalkVertexRepository.findNeighbours(vertex.id)

  override def findNearestSidewalks(coordinate: Coordinate, radius: Double): List[SidewalkEdge] = sidewalkEdgeRepository.findNearestSidewalks(coordinate, radius)
  override def findNearestStreetCrossing(coordinate: Coordinate, radius: Double): List[StreetCrossingEdge] = streetCrossingEdgeRepository.findNearestSidewalks(coordinate, radius)
}

case class EagerSidewalkGraphContainer(override val vertices: List[SidewalkVertex]) extends EagerGeoGraphContainer(vertices)
    with SidewalkGraphContainer with LazyLoggerSupport {

  /**
   * Create a new EagerSidewalkGraphContainer with maximal connected subgraph that this graph contains
   * @return The connected graph
   */
  def purgeSidewalks: EagerSidewalkGraphContainer = {
    logger.info(s"Purge the sidewalk graph in order to get a connected graph")
    GraphUtils.getConnectedComponent(this, EagerSidewalkGraphContainer.apply)
  }

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

  override def findNearestSidewalks(coordinate: Coordinate, radius: Double): List[SidewalkEdge] =
    findNearestPedestrianEdges(coordinate, radius, sidewalkEdges)

  override def findNearestStreetCrossing(coordinate: Coordinate, radius: Double): List[StreetCrossingEdge] =
    findNearestPedestrianEdges(coordinate, radius, streetCrossingEdges)

  protected def findNearestPedestrianEdges[E <: PedestrianEdge](coordinate: Coordinate, radius: Double, edges: List[E]): List[E] = {
    GeoSearch.findNearestByRadius(coordinate, radius, edges,
      (edge: E) ⇒
        Seq(findVertex(edge.vertexStartId).get.coordinate, findVertex(edge.vertexEndId).get.coordinate))
  }

  override def neighbours(vertex: SidewalkVertex): List[SidewalkVertex] = {
    // FIXME meter los neighbours en un cache, por ejemplo usar un TrieMap con vertex.id como key y neighbourIds como value
    val neighbourIds: List[Long] = vertex.edges.map(edge ⇒ if (edge.vertexStartId == vertex.id) edge.vertexEndId else edge.vertexStartId)
    neighbourIds.flatMap(id ⇒ findVertex(id) toList)
  }
}

object EagerSidewalkGraphContainer extends LazyLoggerSupport {

  def createFromDB: EagerSidewalkGraphContainer = {
    logger.info("Getting Sidewalk Graph from the DB")
    EagerSidewalkGraphContainer(SidewalkVertexRepository.findAll)
  }
}
