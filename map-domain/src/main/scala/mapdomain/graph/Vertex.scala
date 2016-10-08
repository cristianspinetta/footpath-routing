package mapdomain.graph

trait Vertex[E <: Edge] {
  val id: Long
  val edges: List[E]

  def getEdgesFor(vertexId: Long): Option[E] = edges.find(_.vertexEndId == vertexId)
  def distanceToNeighbour(vertexId: Long): Double
}

case class GraphVertex[E <: GraphEdge](id: Long, edges: List[E]) extends Vertex[E] {
  def distanceToNeighbour(vertexId: Long): Double = this.getEdgesFor(vertexId).map(_.distance).getOrElse(0)
}

object GraphVertex {
  def createWithEdges(id: Long, edges: List[Int]): GraphVertex[GraphEdge] = GraphVertex(id, edges.map(GraphEdge(id, _)))
  def removeEdge[E <: GraphEdge](source: GraphVertex[E], to: Long): GraphVertex[E] = source.copy(edges = source.edges.filterNot(_.vertexEndId == to))
}

