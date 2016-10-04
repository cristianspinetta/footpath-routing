package mapdomain.graph

import scala.collection.Map

trait GraphContainer[E <: Edge, V <: Vertex[E]] {
  /**
   * Find vertex by ID
   * @param id: Long
   * @return
   */
  def findVertex(id: Long): Option[V]

  def neighbours(vertex: V): List[V]


}

trait InMemoryGraphContainer[E <: Edge, V <: Vertex[E]] extends GraphContainer[E, V] {

  type Self = InMemoryGraphContainer[E, V]

  val vertices: List[V]

  val vertexById: Map[Long, V] = vertices.map(v => v.id -> v) toMap
  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return an option of vertex
   */
  def findVertex(id: Long): Option[V] = vertexById.get(id)

  override def neighbours(vertex: V): List[V] = vertex.edges.flatMap(edge ⇒ findVertex(edge.vertexEndId) toList)

}

class InMemoryGraphContainerImpl[E <: Edge, V <: Vertex[E]](val vertices: List[V]) extends InMemoryGraphContainer[E, V]

object InMemoryGraphContainer {
  def apply[E <: Edge, V <: Vertex[E]](vertices: List[V]): InMemoryGraphContainer[E, V] = new InMemoryGraphContainerImpl(vertices)

  def joinGraphs[E <: Edge, V <: Vertex[E], G <: InMemoryGraphContainer[E, V]](graphs: List[G], constructor: (List[V]) ⇒ G): G = {
    val vertices: List[V] = graphs.flatMap(graph ⇒ graph.vertices)
    constructor(vertices)
  }
}

trait GeoGraphContainer[E <: GeoEdge, V <: GeoVertex[E]] extends GraphContainer[E, V] {
  def findNearest(coordinate: Coordinate): Option[V]
}

object GeoGraphContainer {

  def findNearest[E <: GeoEdge, V <: GeoVertex[E]](vertices: Traversable[V], coordinate: Coordinate): Option[V] = vertices match {
    case Nil ⇒ None
    case list ⇒
      val (closestVertex, _) = list.tail.foldLeft((list.head, list.head.coordinate.distanceTo(coordinate))) {
        case (before @ (partialClosest: V, distanceToBefore: Double), next: V) ⇒
          val distanceToNext: Double = next.coordinate.distanceTo(coordinate)
          if (distanceToNext < distanceToBefore) (next, distanceToNext)
          else before
      }
      Some(closestVertex)
  }

  def neighbours[E <: GeoEdge, V <: GeoVertex[E]](vertex: V)(implicit graph: GraphContainer[E, V]): List[V] = vertex.edges.flatMap(edge ⇒ graph.findVertex(edge.vertexEndId) toList)
}


trait InMemoryGeoGraphContainer[E <: GeoEdge, V <: GeoVertex[E]] extends InMemoryGraphContainer[E, V] with GeoGraphContainer[E, V] {
  //  val constructor: Constructor = (vertices: List[V]) ⇒ new InMemoryGeoGraphContainer(vertices)

  // FIXME reemplazar por GeoSearch
  override def findNearest(coordinate: Coordinate): Option[V] = GeoGraphContainer.findNearest[E, V](vertices, coordinate)
}

class InMemoryGeoGraphContainerImpl[E <: GeoEdge, V <: GeoVertex[E]](val vertices: List[V]) extends InMemoryGeoGraphContainer[E, V]

object InMemoryGeoGraphContainer {
  def apply[E <: GeoEdge, V <: GeoVertex[E]](vertices: List[V]): InMemoryGeoGraphContainer[E, V] = new InMemoryGeoGraphContainerImpl(vertices)
}

object GraphContainer {

  def createEagerGeoGraph(vertexData: Map[Long, (List[Long], Coordinate)]): InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = {
    val vertices: List[GeoVertex[GeoEdge]] = vertexData.toList map {
      case (nodeId, (edgeIds, nodeCoordinate)) ⇒
        new GeoVertex[GeoEdge](nodeId,
          edgeIds.map(neighbourId ⇒ GeoEdge(nodeId, neighbourId, nodeCoordinate.distanceTo(vertexData(neighbourId)._2))),
          nodeCoordinate)
    }
    InMemoryGeoGraphContainer(vertices)
  }
}
