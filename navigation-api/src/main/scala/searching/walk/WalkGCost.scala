package searching.walk

import base.conf.ApiEnvConfig
import mapdomain.sidewalk._
import pathgenerator.graph.GCost
import provider.{ GraphSupport, RampProviderSupport }

case object WalkGCost extends GCost[PedestrianEdge, SidewalkVertex] with ApiEnvConfig with GraphSupport with RampProviderSupport {
  private val costs = configuration.Routing.heuristicCost

  override def calculate(from: SidewalkVertex, to: SidewalkVertex): Double = {
    val edge: PedestrianEdge = from.getEdgesFor(to.id).get
    val distanceCost = from.coordinate.distanceTo(to.coordinate)
    val accessibilityCost = edge match {
      case SidewalkEdge(vertexStartId, vertexEndId, _, _, _, id, isAccessible) ⇒
        if (isAccessible) 0 else costs.inaccessibleSidewalk
      case StreetCrossingEdge(vertexStartId, vertexEndId, _, id, rampStartIdOpt, rampEndIdOpt) ⇒
        rampCost(rampStartIdOpt) + rampCost(rampEndIdOpt)
    }
    distanceCost + accessibilityCost
  }

  protected def rampCost(rampIdOpt: Option[Long]): Int =
    rampIdOpt
      .flatMap(rampProvider.findRamp)
      .filter(_.isAccessible)
      .map(_ ⇒ 0)
      .getOrElse(costs.inaccessibleRamp)
}
