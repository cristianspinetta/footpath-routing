package mapdomain.graph

trait Edge {
  val vertexStart: Long
  val vertexEnd: Long
  val distance: Double
  val directed: Boolean = true

  /**
   * true if have the same path, regardless whether they have the same source and destination.
   *
   * Compare with no orientation, so the edge (x, y) is identical to the edge (y, x).
   * @param to: the edge to compare.
   * @return A Boolean indicating are equal or not.
   */
  def equalDirection(to: Edge): Boolean =
    (vertexStart == to.vertexStart && vertexEnd == to.vertexEnd) ||
      (vertexStart == to.vertexEnd && vertexEnd == to.vertexStart)
}

case class GraphEdge(vertexStart: Long, vertexEnd: Long, distance: Double = 10, override val directed: Boolean = true) extends Edge

class GeoEdge(override val vertexStart: Long, override val vertexEnd: Long, override val distance: Double, override val directed: Boolean = true) extends Edge {
  override def toString: String = s"GeoEdge(vertexStart: $vertexStart, vertexEnd: $vertexEnd, distance: $distance, directed: $directed)"

  // To more idiomatic code
  def retrieveVertexStart[V <: GeoVertex](implicit graph: GraphContainer[V]): Option[V] = graph.findVertex(vertexStart)
  def retrieveVertexEnd[V <: GeoVertex](implicit graph: GraphContainer[V]): Option[V] = graph.findVertex(vertexEnd)
  def retrieveOppositeVertexFor[V <: GeoVertex](vertexId: Long)(implicit graph: GraphContainer[V]): Option[V] = {
    assert(vertexStart == vertexId || vertexEnd == vertexId,
      s"The supplied vertex $vertexId doesn't belong to this edge [vertexStart = $vertexStart, vertexEnd = $vertexEnd]")
    val vertexToSearch: Long = if (vertexStart == vertexId) vertexEnd else vertexStart
    graph.findVertex(vertexToSearch)
  }

  // TODO antes de usarlo en el mapa agregar un id
}

object GeoEdge {

  def apply(vertexStart: Long, vertexEnd: Long, distance: Double, directed: Boolean = true): GeoEdge = new GeoEdge(vertexStart, vertexEnd, distance, directed)

  def apply(vertexStart: Long)(vertexEnd: Long, distance: Double): GeoEdge = new GeoEdge(vertexStart, vertexEnd, distance)
}
