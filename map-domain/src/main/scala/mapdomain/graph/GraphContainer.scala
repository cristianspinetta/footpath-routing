package mapdomain.graph

import mapdomain.utils.GraphUtils
import Stream.cons

trait GraphContainer[V <: Vertex] {
  /**
   * Find vertex by ID
   * @param id: Long
   * @return
   */
  def findVertex(id: Long): Option[V]

  def neighbours(vertex: V): List[V]

}

trait LazyGraphContainer[V <: Vertex] extends GraphContainer[V]

class EagerGraphContainer[V <: Vertex](val vertices: List[V]) extends GraphContainer[V] {

  type Self = EagerGraphContainer[V]

  /**
   * Find vertex by ID
   *
   * @param id : Long
   * @return an option of vertex
   */
  def findVertex(id: Long): Option[V] = vertices.find(_.id == id)

  override def neighbours(vertex: V): List[V] = vertex.edges.flatMap(edge ⇒ findVertex(edge.vertexEndId) toList)

}

object EagerGraphContainer {
  def apply[V <: Vertex](vertices: List[V]): EagerGraphContainer[V] = new EagerGraphContainer(vertices)

  def joinGraphs[V <: Vertex, G <: EagerGraphContainer[V]](graphs: List[G], constructor: (List[V]) ⇒ G): G = {
    val vertices: List[V] = graphs.flatMap(graph ⇒ graph.vertices)
    constructor(vertices)
  }
}

trait GeoGraphContainer[V <: GeoVertex] extends GraphContainer[V] {
  def findNearest(coordinate: Coordinate): Option[V]
}

class EagerGeoGraphContainer[V <: GeoVertex](override val vertices: List[V]) extends EagerGraphContainer[V](vertices) with GeoGraphContainer[V] {
  //  val constructor: Constructor = (vertices: List[V]) ⇒ new EagerGeoGraphContainer(vertices)

  // FIXME reemplazar por GeoSearch
  override def findNearest(coordinate: Coordinate): Option[V] = vertices match {
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
}

trait LazyGeoGraphContainer[V <: GeoVertex] extends LazyGraphContainer[V] with GeoGraphContainer[V]

object EagerGeoGraphContainer {

  def apply[V <: GeoVertex](vertices: List[V]): EagerGeoGraphContainer[V] = new EagerGeoGraphContainer(vertices)
}

object GraphContainer {

  def createEagerGeoGraph(vertexData: Map[Long, (List[Long], Coordinate)]): EagerGeoGraphContainer[GeoVertex] = {
    val vertices: List[GeoVertex] = vertexData.toList map {
      case (nodeId, (edgeIds, nodeCoordinate)) ⇒
        new GeoVertex(nodeId,
          edgeIds.map(neighbourId ⇒ GeoEdge(nodeId, neighbourId, nodeCoordinate.distanceTo(vertexData(neighbourId)._2))),
          nodeCoordinate)
    }
    new EagerGeoGraphContainer(vertices)
  }
}
