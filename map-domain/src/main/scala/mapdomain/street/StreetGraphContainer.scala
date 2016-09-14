package mapdomain.street

import base.LazyLoggerSupport
import mapdomain.graph._
import mapdomain.utils.GraphUtils

trait StreetGraphContainer extends GeoGraphContainer[StreetVertex] {
  def findNearestStreets(coordinate: Coordinate, radius: Double): List[StreetEdge]
}

case class LazyStreetGraphContainer() extends LazyGeoGraphContainer[StreetVertex] with StreetGraphContainer with StreetRepositorySupport {
  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[StreetVertex] = StreetVertexRepository.find(id)

  override def findNearest(coordinate: Coordinate): Option[StreetVertex] = streetVertexRepository.findNearest(coordinate)

  def findNearestVertex(coordinate: Coordinate): Option[StreetVertex] = findNearest(coordinate)

  override def neighbours(vertex: StreetVertex): List[StreetVertex] = streetVertexRepository.findNeighbours(vertex.id)

  override def findNearestStreets(coordinate: Coordinate, radius: Double): List[StreetEdge] = streetEdgeRepository.findNearestStreets(coordinate, radius)
}

case class EagerStreetGraphContainer(override val vertices: List[StreetVertex]) extends EagerGeoGraphContainer(vertices)
    with StreetGraphContainer with LazyLoggerSupport {

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

  /**
   * Create a new EagerStreetGraphContainer with maximal connected subgraph that this graph contains
   * @return The connected graph
   */
  def purgeStreets: EagerStreetGraphContainer = {
    logger.info(s"Purge the street graph in order to get a connected graph")
    GraphUtils.getConnectedComponent(this, EagerStreetGraphContainer.apply)
  }
}

object EagerStreetGraphContainer extends LazyLoggerSupport {
  def createFromDB: EagerStreetGraphContainer = {
    logger.info("Getting Street Graph from the DB")
    EagerStreetGraphContainer(StreetVertexRepository.findAll)
  }
}
