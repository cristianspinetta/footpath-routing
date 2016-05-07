package pathgenerator.graph

case class GraphContainer[N <: Node](nodes: List[N]) {

  def findNode(id: Long): Option[N] = nodes.find(_.id == id) // TODO: replace by a DB query

}

object GraphContainer {

  def createGeoNodes(nodeData: Map[Long, (List[Long], Coordinate)]): GraphContainer[GeoNode] = {

    val nodes: List[GeoNode] = nodeData.toList map {
      case (nodeId, (edgeIds, nodeCoordinate)) ⇒
        GeoNode(nodeId,
          edgeIds.map(neighbourId ⇒ GeoEdge(nodeId, neighbourId, nodeCoordinate.distanceTo(nodeData(neighbourId)._2))),
          nodeCoordinate)
    }

    new GraphContainer(nodes)
  }
}
