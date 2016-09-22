package mapdomain.graph

import scala.collection.Map

trait GraphContainer[V <: Vertex] {
  /**
   * Find vertex by ID
   * @param id: Long
   * @return
   */
  def findVertex(id: Long): Option[V]

  def neighbours(vertex: V): List[V]


}

trait InMemoryGraphContainer[V <: Vertex] extends GraphContainer[V] {

  type Self = InMemoryGraphContainer[V]

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

class InMemoryGraphContainerImpl[V <: Vertex](val vertices: List[V]) extends InMemoryGraphContainer[V]

object InMemoryGraphContainer {
  def apply[V <: Vertex](vertices: List[V]): InMemoryGraphContainer[V] = new InMemoryGraphContainerImpl(vertices)

  def joinGraphs[V <: Vertex, G <: InMemoryGraphContainer[V]](graphs: List[G], constructor: (List[V]) ⇒ G): G = {
    val vertices: List[V] = graphs.flatMap(graph ⇒ graph.vertices)
    constructor(vertices)
  }
}

trait GeoGraphContainer[V <: GeoVertex] extends GraphContainer[V] {
  def findNearest(coordinate: Coordinate): Option[V]
}

object GeoGraphContainer {

  def findNearest[V <: GeoVertex](vertices: Traversable[V], coordinate: Coordinate): Option[V] = vertices match {
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

  def neighbours[V <: GeoVertex](vertex: V)(implicit graph: GraphContainer[V]): List[V] = vertex.edges.flatMap(edge ⇒ graph.findVertex(edge.vertexEndId) toList)
}


trait InMemoryGeoGraphContainer[V <: GeoVertex] extends InMemoryGraphContainer[V] with GeoGraphContainer[V] {
  //  val constructor: Constructor = (vertices: List[V]) ⇒ new InMemoryGeoGraphContainer(vertices)

  // FIXME reemplazar por GeoSearch
  override def findNearest(coordinate: Coordinate): Option[V] = GeoGraphContainer.findNearest(vertices, coordinate)
}

class InMemoryGeoGraphContainerImpl[V <: GeoVertex](val vertices: List[V]) extends InMemoryGeoGraphContainer[V]

object InMemoryGeoGraphContainer {
  def apply[V <: GeoVertex](vertices: List[V]): InMemoryGeoGraphContainer[V] = new InMemoryGeoGraphContainerImpl(vertices)
}

object GraphContainer {

  def createEagerGeoGraph(vertexData: Map[Long, (List[Long], Coordinate)]): InMemoryGeoGraphContainer[GeoVertex] = {
    val vertices: List[GeoVertex] = vertexData.toList map {
      case (nodeId, (edgeIds, nodeCoordinate)) ⇒
        new GeoVertex(nodeId,
          edgeIds.map(neighbourId ⇒ GeoEdge(nodeId, neighbourId, nodeCoordinate.distanceTo(vertexData(neighbourId)._2))),
          nodeCoordinate)
    }
    InMemoryGeoGraphContainer(vertices)
  }
}
