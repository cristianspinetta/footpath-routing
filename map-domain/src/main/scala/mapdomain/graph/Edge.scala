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

case class EdgeReference[E <: Edge, V <: Vertex[E]](vertexStart: V, vertexEnd: V, edge: E)
