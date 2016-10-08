package mapdomain.graph

trait GeoEdge extends Edge {

  // To more idiomatic code
  def retrieveVertexStart[E <: GeoEdge, V <: GeoVertex[E]](implicit graph: GraphContainer[E, V]): Option[V] = graph.findVertex(vertexStartId)
  def retrieveVertexEnd[E <: GeoEdge, V <: GeoVertex[E]](implicit graph: GraphContainer[E, V]): Option[V] = graph.findVertex(vertexEndId)
  def retrieveOppositeVertexFor[E <: GeoEdge, V <: GeoVertex[E]](vertexId: Long)(implicit graph: GraphContainer[E, V]): Option[V] = {
    assert(vertexStartId == vertexId || vertexEndId == vertexId,
      s"The supplied vertex $vertexId doesn't belong to this edge [vertexStartId = $vertexStartId, vertexEndId = $vertexEndId]")
    val vertexToSearch: Long = if (vertexStartId == vertexId) vertexEndId else vertexStartId
    graph.findVertex(vertexToSearch)
  }

  override def toString: String = s"GeoEdge(vertexStartId: $vertexStartId, vertexEndId: $vertexEndId, distance: $distance, directed: $directed)"
}

object GeoEdge {

  def apply(vertexStart: Long, vertexEnd: Long, distance: Double, directed: Boolean = true): GeoEdge = new GeoEdgeImpl(vertexStart, vertexEnd, distance, directed)

  def apply(vertexStart: Long)(vertexEnd: Long, distance: Double): GeoEdge = new GeoEdgeImpl(vertexStart, vertexEnd, distance)
}

class GeoEdgeImpl(val vertexStartId: Long, val vertexEndId: Long, val distance: Double, override val directed: Boolean = true) extends GeoEdge
