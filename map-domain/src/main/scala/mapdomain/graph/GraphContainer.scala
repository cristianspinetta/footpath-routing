package mapdomain.graph

class GraphContainer[N <: Vertex](val vertices: List[N]) {

  /**
   * Find vertex by ID
   * @param id
   * @return
   */
  def findVertex(id: Long): Option[N] = vertices.find(_.id == id) // TODO: replace by a DB query

  def copy(vertices: List[N]): GraphContainer[N] = GraphContainer(vertices)
  def copy(): GraphContainer[N] = GraphContainer(vertices)

}

object GraphContainer {

  def apply[N <: Vertex](vertices: List[N]): GraphContainer[N] = new GraphContainer(vertices)

  def createGeoNodes(nodeData: Map[Long, (List[Long], Coordinate)]): GraphContainer[GeoVertex] = {

    val nodes: List[GeoVertex] = nodeData.toList map {
      case (nodeId, (edgeIds, nodeCoordinate)) ⇒
        new GeoVertex(nodeId,
          edgeIds.map(neighbourId ⇒ GeoEdge(nodeId, neighbourId, nodeCoordinate.distanceTo(nodeData(neighbourId)._2))),
          nodeCoordinate)
    }

    new GraphContainer(nodes)
  }

  def findClosestVertex[V <: GeoVertex](graph: GraphContainer[V], coordinate: Coordinate): Option[(V, Double)] = graph.vertices match {
    case Nil ⇒ None
    case list ⇒
      val (closestVertex, distance) = list.tail.foldLeft((list.head, list.head.coordinate.distanceTo(coordinate))) {
        case (before @ (partialClosest: V, distanceToBefore: Double), next: V) ⇒
          val distanceToNext: Double = next.coordinate.distanceTo(coordinate)
          if (distanceToNext < distanceToBefore) (next, distanceToNext)
          else before
      }
      Some((closestVertex, distance))
  }

  def joinGraphs[V <: Vertex](graphs: List[GraphContainer[V]]) = {
    val vertices: List[V] = graphs.flatMap(graph ⇒ graph.vertices)
    GraphContainer(vertices)
  }
}
