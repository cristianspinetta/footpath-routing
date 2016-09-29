package mapdomain.sidewalk

import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph._
import mapdomain.utils.GraphUtils

import scala.collection.concurrent.TrieMap

trait SidewalkGraphContainer extends GeoGraphContainer[PedestrianEdge, SidewalkVertex] {
  def findNearestSidewalks(coordinate: Coordinate, radius: Double): List[SidewalkEdge]
  def findNearestStreetCrossing(coordinate: Coordinate, radius: Double): List[StreetCrossingEdge]
  def vertices: List[SidewalkVertex]
}

case class LazySidewalkGraphContainer() extends SidewalkGraphContainer with SidewalkRepositorySupport {

  protected val vertexById = new TrieMap[Long, SidewalkVertex]()
  protected val totalVertices: Long = sidewalkVertexRepository.totalVertices

  override def vertices: List[SidewalkVertex] = {
    if (totalVertices != vertexById.size) {
      vertexById.keys
    }
    SidewalkVertexRepository.findAll
  }
  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[SidewalkVertex] = {
    vertexById.get(id) orElse {
      val maybeVertex: Option[SidewalkVertex] = SidewalkVertexRepository.find(id)
      maybeVertex foreach (v ⇒ vertexById += (v.id -> v))
      maybeVertex
    }
  }

  override def findNearest(coordinate: Coordinate): Option[SidewalkVertex] = sidewalkVertexRepository.findNearest(coordinate)

  override def neighbours(vertex: SidewalkVertex): List[SidewalkVertex] = sidewalkVertexRepository.findNeighbours(vertex.id)

  override def findNearestSidewalks(coordinate: Coordinate, radius: Double): List[SidewalkEdge] = sidewalkEdgeRepository.findNearestSidewalks(coordinate, radius)
  override def findNearestStreetCrossing(coordinate: Coordinate, radius: Double): List[StreetCrossingEdge] = streetCrossingEdgeRepository.findNearestSidewalks(coordinate, radius)
}

case class InMemorySidewalkGraphContainer(vertices: List[SidewalkVertex]) extends SidewalkGraphContainer with InMemoryGraphContainer[PedestrianEdge, SidewalkVertex] with LazyLoggerSupport with MeterSupport {

  var ramps: List[Ramp] = List()

  protected val totalVertices: Long = vertices.size

  lazy val sidewalkEdges: List[SidewalkEdge] = { // FIXME esto quedo medio cualquiera, reveer!
    (for (vertex ← vertices; edge ← vertex.sidewalkEdges) yield (edge.keyValue, edge))
      .groupBy { case (key, _) ⇒ key }
      .map { case (_, (_, edge) :: edgesTail) ⇒ edge }
      .toList
  }
  lazy val streetCrossingEdges: List[StreetCrossingEdge] = { // FIXME esto quedo medio cualquiera, reveer!
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
    /*val edges = vertex.sidewalkEdges.filter(e => e.isAccessible) ++ vertex.streetCrossingEdges.filter(sce => {
                          val startRamp = findRamp(sce.rampStartId)
                          val endRamp = findRamp(sce.rampEndId)
                          if(startRamp.isEmpty || endRamp.isEmpty)
                            false
                          else
                            startRamp.get.isAccessible && endRamp.get.isAccessible
                        })*/
    val neighbourIds: List[Long] = vertex.edges.map(edge ⇒ if (edge.vertexStartId == vertex.id) edge.vertexEndId else edge.vertexStartId)
    neighbourIds.flatMap(id ⇒ findVertex(id) toList)
  }

  private def findRamp(id: Option[Long]): Option[Ramp] = id match {
    case Some(i) => ramps.find(r => r.id.get == i)
    case None => None
  }

  override def findNearest(coordinate: Coordinate): Option[SidewalkVertex] = GeoGraphContainer.findNearest[PedestrianEdge, SidewalkVertex](vertices, coordinate)

  /**
    * Find vertex by ID
    *
    * @param id : Long
    * @return
    */
  override def findVertex(id: Long): Option[SidewalkVertex] = vertexById.get(id)

  /**
   * Create a new InMemorySidewalkGraphContainer with maximal connected subgraph that this graph contains
   * @return The connected graph
   */
  def purgeSidewalks: InMemorySidewalkGraphContainer = withTimeLogging({
    logger.info(s"Purge the sidewalk graph in order to get a connected graph")
    val container = GraphUtils.getConnectedComponent[PedestrianEdge, SidewalkVertex, InMemorySidewalkGraphContainer](this, InMemorySidewalkGraphContainer.apply)
    container.ramps = ramps
    container
  }, (time: Long) => logger.info(s"Sidewalk graph was purged in $time ms."))
}

object InMemorySidewalkGraphContainer extends LazyLoggerSupport with MeterSupport {

  def createFromDB: InMemorySidewalkGraphContainer = withTimeLogging({
    logger.info("Getting Sidewalk Graph from DB")
    val graph = InMemorySidewalkGraphContainer(SidewalkVertexRepository.findAll)
    graph.ramps = RampRepository.findAll
    graph
  }, (time: Long) => logger.info(s"Loading Sidewalk Graph from DB finished in $time ms."))
}
