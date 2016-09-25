package mapdomain.street

import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph._
import mapdomain.utils.GraphUtils

import scala.collection.concurrent.TrieMap

trait StreetGraphContainer extends GeoGraphContainer[StreetVertex] {
  def findNearestStreets(coordinate: Coordinate, radius: Double): List[StreetEdge]
  def vertices: List[StreetVertex]
}

case class LazyStreetGraphContainer() extends StreetGraphContainer with StreetRepositorySupport {

  protected val vertexById = new TrieMap[Long, StreetVertex]()
  protected val totalVertices: Long = streetVertexRepository.totalVertices

  override def vertices: List[StreetVertex] = {
    if (totalVertices != vertexById.size) {
      vertexById.keys
    }
    StreetVertexRepository.findAll
  }

  override def findNearestStreets(coordinate: Coordinate, radius: Double): List[StreetEdge] = streetEdgeRepository.findNearestStreets(coordinate, radius)

  override def findNearest(coordinate: Coordinate): Option[StreetVertex] = streetVertexRepository.findNearest(coordinate)

  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[StreetVertex] = {
    vertexById.get(id) orElse {
      val maybeVertex: Option[StreetVertex] = StreetVertexRepository.find(id)
      maybeVertex foreach (v ⇒ vertexById += (v.id -> v))
      maybeVertex
    }
  }

  override def neighbours(vertex: StreetVertex): List[StreetVertex] = streetVertexRepository.findNeighbours(vertex.id) // FIXME usar mapa para cachear
}

case class InMemoryStreetGraphContainer(vertices: List[StreetVertex]) extends StreetGraphContainer with InMemoryGeoGraphContainer[StreetVertex] with LazyLoggerSupport with MeterSupport {

  protected val totalVertices: Long = vertices.size

  override def findNearest(coordinate: Coordinate): Option[StreetVertex] = GeoGraphContainer.findNearest(vertices, coordinate)

  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[StreetVertex] = vertexById.get(id)

  override def neighbours(vertex: StreetVertex): List[StreetVertex] = GeoGraphContainer.neighbours(vertex)(this)

  lazy val streets: List[StreetEdge] = for {
    vertex ← vertices
    edge ← vertex.edges
  } yield edge

  override def findNearestStreets(coordinate: Coordinate, radius: Double): List[StreetEdge] = {
    GeoSearch.findNearestByRadius(coordinate,
      radius, streets,
      (street: StreetEdge) ⇒
        Seq(findVertex(street.vertexStartId).get.coordinate, findVertex(street.vertexEndId).get.coordinate))
  }
}

object InMemoryStreetGraphContainer extends LazyLoggerSupport with MeterSupport {

  def createFromDB: InMemoryStreetGraphContainer = withTimeLogging({
    logger.info("Getting Street Graph from DB")
    InMemoryStreetGraphContainer(StreetVertexRepository.findAll)
  }, (time: Long) ⇒ logger.info(s"Loading Street Graph finished from DB in $time ms."))
}

case class UnsavedStreetGraphContainer(vertices: List[UnsavedStreetVertex]) extends InMemoryGeoGraphContainer[UnsavedStreetVertex] with LazyLoggerSupport with MeterSupport {

  /**
   * Create a new InMemoryStreetGraphContainer with maximal connected subgraph that this graph contains
   * @return The connected graph
   */
  def purgeStreets: UnsavedStreetGraphContainer = withTimeLogging({
    logger.info(s"Purge the unsaved street graph in order to get a connected graph")
    GraphUtils.getConnectedComponent(this, UnsavedStreetGraphContainer.apply)
  }, (time: Long) ⇒ logger.info(s"Street graph was purged in $time ms."))

}
