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

class GeoVertex[E <: GeoEdge](override val id: Long, override val edges: List[E], val coordinate: Coordinate) extends Vertex[E] {
  //  def removeEdgeAt(to: Long): GeoVertex = this.copy(edges = edges.filterNot(_.vertexEndId == to))
  def distanceToNeighbour(vertexId: Long): Double = this.getEdgesFor(vertexId).map(_.distance).getOrElse(0) // TODO report access by getOrElse
  def distanceTo[E <: GeoEdge](vertex: GeoVertex[E]): Double = this.coordinate.distanceTo(vertex.coordinate)
  //  def removeEdge(source: GeoVertex, to: Long): GeoVertex = source.copy(edges = source.edges.filterNot(_.vertexEndId == to))

  override def toString: String = s"GeoVertex(id: $id, edges: $edges, coordinate: $coordinate)"
}
object GeoVertex {
  def createWithEdges(id: Long, edges: List[(Long, Coordinate)], coordinate: Coordinate): GeoVertex[GeoEdge] = {
    val edge: (Long, Double) ⇒ GeoEdge = GeoEdge(id)

    new GeoVertex(id, edges.map {
      case (neighbourId, neighbourCoordinate) ⇒
        edge(neighbourId, neighbourCoordinate.distanceTo(coordinate))
    }, coordinate)
  }
}
