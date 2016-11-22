package searching

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import cats.data.{ Xor, XorT }
import mapdomain.graph.Coordinate
import mapdomain.publictransport.Stop
import model.{ BusPath, Path, PathCoordinate, PathDescription }
import provider.PublicTransportProviderSupport
import searching.SearchRoutingErrors.{ NoPathBetweenStops, SearchRoutingError }
import searching.walk.WalkRouteSearcherSupport
import utils.JsonUtils

import scala.concurrent.{ ExecutionContext, Future }

private[searching] object PathBuilders {

  trait PathBuilder {
    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path]
    def cost: Double
  }

  case class TransportPathBuilder(travelInfoId: Long, stopFrom: Stop, stopTo: Stop) extends PathBuilder with ApiEnvConfig with LazyLoggerSupport with PublicTransportProviderSupport {

    private val costs = configuration.Routing.heuristicCost
    private lazy val travelInfo = publicTransportProvider.findTravelInfo(stopFrom.travelInfoId)

    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
      Future[Xor[SearchRoutingError, Path]] {
        val coordinates = publicTransportProvider.getPathBetweenStops(stopFrom, stopTo).map(PathCoordinate(_))
        val transportDescription = s"${travelInfo.`type`} - Line ${travelInfo.name} - Branch ${travelInfo.branch} - ${travelInfo.sentido}"
        // FIXME add more info on PathDescription
        Xor.Right(Path(coordinates, PathDescription(BusPath, s"$transportDescription - From", s"$transportDescription - To"),
          extractIncidents))
      } recover {
        case exc: Throwable ⇒
          logger.error(s"An error occur trying to build the path between stops. $stopFrom, $stopTo", exc)
          Xor.Left(NoPathBetweenStops)
      }
    }

    private lazy val accessibilityStopCost: Int = ((if (stopFrom.isAccessible) 0 else 1) + (if (stopTo.isAccessible) 0 else 1)) * costs.inaccessibleStop
    private lazy val busCost: Int = (if (travelInfo.`type` != "SUBWAY") 1 else 0) * costs.bus
    private lazy val sequenceCost: Long = scala.math.abs(stopTo.sequence - stopFrom.sequence) * costs.stop

    lazy val cost: Double = sequenceCost + costs.combination + accessibilityStopCost + busCost

    private lazy val extractIncidents: List[PedestrianIncident] = {
      List(stopFrom, stopTo)
        .filter(!_.isAccessible)
        .map(stop ⇒ PedestrianIncident(StopIncidentType, Some(stop.coordinate)))
    }
  }

  case class WalkPathBuilder(from: Coordinate, to: Coordinate, addStartLine: Boolean = false, addEndLine: Boolean = false) extends PathBuilder with ApiEnvConfig with WalkRouteSearcherSupport {
    private val costs = configuration.Routing.heuristicCost
    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = {
      this.walkRouteSearcher.search(coordinateFrom = from, coordinateTo = to, addStartLine = addStartLine, addEndLine = addEndLine)
    }

    lazy val cost: Double = from.distanceTo(to) * costs.distanceByKm
  }

  case class WalkPathCombinationBuilder(transportFromStopTo: Stop, transportToStopFrom: Stop) extends PathBuilder with ApiEnvConfig with LazyLoggerSupport with PublicTransportProviderSupport {

    private val costs = configuration.Routing.heuristicCost

    private val transportFromStopToId: Long = transportFromStopTo.id
    private val toTravelInfoId: Long = transportToStopFrom.travelInfoId
    private lazy val combinationPath = publicTransportProvider.getCombinationWalkPathByStopAndTravelInfo(transportFromStopToId, toTravelInfoId)

    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
      Future[Xor[SearchRoutingError, Path]] {
        Xor.Right(JsonUtils.fromJson(combinationPath.walkPath))
      } recoverWith {
        case exc: Throwable ⇒
          logger.error(s"An error occur trying to build the path for combination. [transportFromStopToId = $transportFromStopToId, toTravelInfoId = $toTravelInfoId]", exc)
          WalkPathBuilder(transportFromStopTo.coordinate, transportToStopFrom.coordinate, addStartLine = true, addEndLine = true)
            .build
            .value
      }
    }

    lazy val cost: Double = transportFromStopTo.coordinate.distanceTo(transportToStopFrom.coordinate) * costs.distanceByKm
  }

}
