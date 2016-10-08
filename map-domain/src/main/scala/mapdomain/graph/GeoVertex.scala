package mapdomain.graph

trait GeoVertex[E <: GeoEdge] extends Vertex[E]{
  val coordinate: Coordinate

  //  def removeEdgeAt(to: Long): GeoVertex = this.copy(edges = edges.filterNot(_.vertexEndId == to))
  def distanceToNeighbour(vertexId: Long): Double = this.getEdgesFor(vertexId).map(_.distance).getOrElse(0) // TODO report access by getOrElse
  def distanceTo[E <: GeoEdge](vertex: GeoVertex[E]): Double = this.coordinate.distanceTo(vertex.coordinate)
  //  def removeEdge(source: GeoVertex, to: Long): GeoVertex = source.copy(edges = source.edges.filterNot(_.vertexEndId == to))

  override def toString: String = s"GeoVertex(id: $id, edges: $edges, coordinate: $coordinate)"
}

class GeoVertexImpl[E <: GeoEdge](override val id: Long, override val edges: List[E], val coordinate: Coordinate) extends GeoVertex[E]

object GeoVertex {
  def createWithEdges(id: Long, edges: List[(Long, Coordinate)], coordinate: Coordinate): GeoVertex[GeoEdge] = {
    val edge: (Long, Double) ⇒ GeoEdge = GeoEdge(id)

    GeoVertex(id, edges.map {
      case (neighbourId, neighbourCoordinate) ⇒
        edge(neighbourId, neighbourCoordinate.distanceTo(coordinate))
    }, coordinate)
  }

  def apply[E <: GeoEdge](id: Long, edges: List[E], coordinate: Coordinate): GeoVertex[E] = new GeoVertexImpl(id, edges,coordinate)
}
