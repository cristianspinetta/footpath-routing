package mapdomain.graph

trait Edge {
  val vertexStartId: Long
  val vertexEndId: Long
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
    (vertexStartId == to.vertexStartId && vertexEndId == to.vertexEndId) ||
      (vertexStartId == to.vertexEndId && vertexEndId == to.vertexStartId)
}

case class GraphEdge(vertexStartId: Long, vertexEndId: Long, distance: Double = 10, override val directed: Boolean = true) extends Edge

class GeoEdge(override val vertexStartId: Long, override val vertexEndId: Long, override val distance: Double, override val directed: Boolean = true) extends Edge {
  override def toString: String = s"GeoEdge(vertexStartId: $vertexStartId, vertexEndId: $vertexEndId, distance: $distance, directed: $directed)"

  // To more idiomatic code
  def retrieveVertexStart[E <: GeoEdge, V <: GeoVertex[E]](implicit graph: GraphContainer[E, V]): Option[V] = graph.findVertex(vertexStartId)
  def retrieveVertexEnd[E <: GeoEdge, V <: GeoVertex[E]](implicit graph: GraphContainer[E, V]): Option[V] = graph.findVertex(vertexEndId)
  def retrieveOppositeVertexFor[E <: GeoEdge, V <: GeoVertex[E]](vertexId: Long)(implicit graph: GraphContainer[E, V]): Option[V] = {
    assert(vertexStartId == vertexId || vertexEndId == vertexId,
      s"The supplied vertex $vertexId doesn't belong to this edge [vertexStartId = $vertexStartId, vertexEndId = $vertexEndId]")
    val vertexToSearch: Long = if (vertexStartId == vertexId) vertexEndId else vertexStartId
    graph.findVertex(vertexToSearch)
  }

  // TODO antes de usarlo en el mapa agregar un id
}

object GeoEdge {

  def apply(vertexStart: Long, vertexEnd: Long, distance: Double, directed: Boolean = true): GeoEdge = new GeoEdge(vertexStart, vertexEnd, distance, directed)

  def apply(vertexStart: Long)(vertexEnd: Long, distance: Double): GeoEdge = new GeoEdge(vertexStart, vertexEnd, distance)
}
