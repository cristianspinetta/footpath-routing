package mapdomain.street

import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph._
import mapdomain.repository.street.{ StreetRepositorySupport, StreetVertexRepository }
import mapdomain.utils.GraphUtils

import scala.collection.Map
import scala.collection.concurrent.TrieMap

trait StreetGraphContainer extends GeoGraphContainer[StreetEdge, StreetVertex[StreetEdge]] {
  def findNearestStreets(coordinate: Coordinate, radius: Double): List[StreetEdge]
  def vertices: List[StreetVertex[StreetEdge]]
}

case class LazyStreetGraphContainer() extends StreetGraphContainer with StreetRepositorySupport {

  protected val vertexById = new TrieMap[Long, StreetVertex[StreetEdge]]()
  protected val totalVertices: Long = streetVertexRepository.totalVertices

  override def vertices: List[StreetVertex[StreetEdge]] = {
    if (totalVertices != vertexById.size) {
      vertexById.keys
    }
    StreetVertexRepository.findAll
  }

  override def findNearestStreets(coordinate: Coordinate, radius: Double): List[StreetEdge] = streetEdgeRepository.findNearestStreets(coordinate, radius)

  override def findNearest(coordinate: Coordinate): Option[StreetVertex[StreetEdge]] = streetVertexRepository.findNearest(coordinate)

  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[StreetVertex[StreetEdge]] = {
    vertexById.get(id) orElse {
      val maybeVertex = StreetVertexRepository.find(id)
      maybeVertex foreach (v ⇒ vertexById += (v.id -> v))
      maybeVertex
    }
  }

  override def neighbours(vertex: StreetVertex[StreetEdge]): List[StreetVertex[StreetEdge]] = streetVertexRepository.findNeighbours(vertex.id) // FIXME usar mapa para cachear
}

case class InMemoryStreetGraphContainer(override val vertexById: Map[Long, StreetVertex[StreetEdge]]) extends StreetGraphContainer with InMemoryGeoGraphContainer[StreetEdge, StreetVertex[StreetEdge]] with LazyLoggerSupport with MeterSupport {

  val vertices: List[StreetVertex[StreetEdge]] = vertexById.values.toList

  protected val totalVertices: Long = vertices.size

  override def findNearest(coordinate: Coordinate): Option[StreetVertex[StreetEdge]] = GeoGraphContainer.findNearest[StreetEdge, StreetVertex[StreetEdge]](vertices, coordinate)

  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[StreetVertex[StreetEdge]] = vertexById.get(id)

  override def neighbours(vertex: StreetVertex[StreetEdge]): List[StreetVertex[StreetEdge]] = GeoGraphContainer.neighbours[StreetEdge, StreetVertex[StreetEdge]](vertex)(this)

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
  def apply(vertices: List[StreetVertex[StreetEdge]]): InMemoryStreetGraphContainer = InMemoryStreetGraphContainer(vertices.map(v ⇒ (v.id, v)) toMap)
}

case class UnsavedStreetGraphContainer(vertices: List[UnsavedStreetVertex]) extends InMemoryGeoGraphContainer[StreetEdgeUnsaved, UnsavedStreetVertex] with LazyLoggerSupport with MeterSupport {

  val vertexById: Map[Long, UnsavedStreetVertex] = vertices.map(v ⇒ v.id -> v) toMap

  /**
   * Create a new InMemoryStreetGraphContainer with maximal connected subgraph that this graph contains
   * @return The connected graph
   */
  def purgeStreets: UnsavedStreetGraphContainer = withTimeLogging({
    logger.info(s"Purge the unsaved street graph in order to get a connected graph")
    GraphUtils.getConnectedComponent[StreetEdgeUnsaved, UnsavedStreetVertex, UnsavedStreetGraphContainer](this, UnsavedStreetGraphContainer.apply)
  }, (time: Long) ⇒ logger.info(s"Street graph was purged in $time ms."))

}
