package searching

import mapdomain.sidewalk._
import pathgenerator.graph.Heuristic
import provider.GraphSupport

case class WalkHeuristic(referenceNode: SidewalkVertex) extends Heuristic[PedestrianEdge, SidewalkVertex] with GraphSupport {
  import WalkHeuristic._

  override def apply(node: SidewalkVertex): Double = referenceNode.coordinate.distanceTo(node.coordinate)

  override def apply(from: SidewalkVertex, to: SidewalkVertex): Double = {
    val edge: PedestrianEdge = from.getEdgesFor(to.id).get
    apply(to) + (edge match {
      case SidewalkEdge(vertexStartId, vertexEndId, _, _, _, id, isAccessible) ⇒
        distance(vertexStartId, vertexEndId) + (if (isAccessible) 0 else inaccesibleWeight)
      case StreetCrossingEdge(vertexStartId, vertexEndId, _, id, rampStartIdOpt, rampEndIdOpt) ⇒
        distance(vertexStartId, vertexEndId) + rampStartIdOpt.map(_ ⇒ 0).getOrElse(inaccesibleWeight) + rampEndIdOpt.map(_ ⇒ 0).getOrElse(inaccesibleWeight)
    })
  }

  protected def distance(vertexStartId: Long, vertexEndId: Long): Double = {
    val vertexStart: SidewalkVertex = graphs.sidewalk.findVertex(vertexStartId).get
    val vertexEnd: SidewalkVertex = graphs.sidewalk.findVertex(vertexEndId).get
    vertexStart.coordinate.distanceTo(vertexEnd.coordinate)
  }
}

object WalkHeuristic {
  val inaccesibleWeight = 100

}
