package searching

import mapdomain.sidewalk._
import pathgenerator.graph.{ GCost, Heuristic }
import provider.GraphSupport

case object WalkGCost extends GCost[PedestrianEdge, SidewalkVertex] with GraphSupport {
  val inaccesibleSidwewalkWeight = 10
  val inaccesibleRampWeight = 5

  override def calculate(from: SidewalkVertex, to: SidewalkVertex): Double = {
    val edge: PedestrianEdge = from.getEdgesFor(to.id).get
    val distanceCost = from.coordinate.distanceTo(to.coordinate)
    val accessibilityCost = edge match {
      case SidewalkEdge(vertexStartId, vertexEndId, _, _, _, id, isAccessible) ⇒
        if (isAccessible) 0 else inaccesibleSidwewalkWeight
      case StreetCrossingEdge(vertexStartId, vertexEndId, _, id, rampStartIdOpt, rampEndIdOpt) ⇒
        rampStartIdOpt.map(_ ⇒ 0).getOrElse(inaccesibleRampWeight) +
          rampEndIdOpt.map(_ ⇒ 0).getOrElse(inaccesibleRampWeight)
    }
    distanceCost + accessibilityCost
  }
}
