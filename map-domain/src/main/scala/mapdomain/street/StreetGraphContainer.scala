package mapdomain.street

import mapdomain.graph.{ Coordinate, EagerGeoGraphContainer, GeoGraphContainer, LazyGeoGraphContainer }

trait StreetGraphContainer[V <: StreetVertex] extends GeoGraphContainer[V]

case class LazyStreetGraphContainer() extends LazyGeoGraphContainer[StreetVertex] with StreetGraphContainer[StreetVertex] with StreetRepositorySupport {
  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return
   */
  override def findVertex(id: Long): Option[StreetVertex] = StreetVertexRepository.find(id)

  override def findNearest(coordinate: Coordinate): Option[StreetVertex] = streetVertexRepository.findNearest(coordinate)

  def findNearestVertex(coordinate: Coordinate): Option[StreetVertex] = findNearest(coordinate)

  override def neighbours(vertex: StreetVertex): Seq[StreetVertex] = ???
}

case class EagerStreetGraphContainer(override val vertices: List[StreetVertex]) extends EagerGeoGraphContainer(vertices) {
  override val constructor: Constructor = (vertices: List[StreetVertex]) ⇒ EagerStreetGraphContainer(vertices)
}
