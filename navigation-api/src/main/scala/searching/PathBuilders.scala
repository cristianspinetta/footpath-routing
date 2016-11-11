package searching

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import cats.data.{ Xor, XorT }
import mapdomain.graph.Coordinate
import mapdomain.publictransport.Stop
import model.{ BusPath, Path, PathDescription }
import provider.PublicTransportProviderSupport
import searching.SearchRoutingErrors.{ NoPathBetweenStops, SearchRoutingError }
import utils.JsonUtils

import scala.concurrent.{ ExecutionContext, Future }

private[searching] object PathBuilders {

  trait PathBuilder {
    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path]
    def cost: Double
  }

  case class TransportPathBuilder(travelInfoId: Long, stopFrom: Stop, stopTo: Stop) extends PathBuilder with ApiEnvConfig with LazyLoggerSupport with PublicTransportProviderSupport {

    private val costs = configuration.Routing.heuristicCost

    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
      Future[Xor[SearchRoutingError, Path]] {
        val coordinates = publicTransportProvider.getPathBetweenStops(stopFrom, stopTo)
        val travelInfo = publicTransportProvider.findTravelInfo(stopFrom.travelInfoId)
        val transportDescription = s"${travelInfo.`type`} - Line ${travelInfo.name} - Branch ${travelInfo.branch} - ${travelInfo.sentido}"
        // FIXME add more info on PathDescription
        Xor.Right(Path(coordinates, PathDescription(BusPath, s"$transportDescription - From", s"$transportDescription - To")))
      } recover {
        case exc: Throwable ⇒
          logger.error(s"An error occur trying to build the path between stops. $stopFrom, $stopTo", exc)
          Xor.Left(NoPathBetweenStops)
      }
    }

    lazy val cost: Double = (stopTo.sequence - stopFrom.sequence) * costs.stop + costs.combination + ((if (stopFrom.isAccessible) 1 else 0) + (if (stopTo.isAccessible) 1 else 0) * costs.inaccessibleStop)
  }

  case class WalkPathBuilder(from: Coordinate, to: Coordinate) extends PathBuilder with ApiEnvConfig with WalkRouteSearcherSupport {
    private val costs = configuration.Routing.heuristicCost
    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = {
      this.walkRouteSearcher.search(from, to)
    }

    lazy val cost: Double = from.distanceTo(to) * costs.distanceByKm
  }

  case class WalkPathCombinationBuilder(transportFromStopTo: Stop, transportToStopFrom: Stop) extends PathBuilder with ApiEnvConfig with LazyLoggerSupport with PublicTransportProviderSupport {

    private val costs = configuration.Routing.heuristicCost

    private val transportFromStopToId: Long = transportFromStopTo.id
    private val toTravelInfoId: Long = transportToStopFrom.travelInfoId
    private lazy val combination = publicTransportProvider.getCombinationByStopAndTravelInfo(transportFromStopToId, toTravelInfoId)

    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
      Future[Xor[SearchRoutingError, Path]] {
        Xor.Right(JsonUtils.fromJson(combination.walkPath.get))
      } recoverWith {
        case exc: Throwable ⇒
          logger.error(s"An error occur trying to build the path for combination. [transportFromStopToId = $transportFromStopToId, toTravelInfoId = $toTravelInfoId]", exc)
          WalkPathBuilder(transportFromStopTo.coordinate, transportToStopFrom.coordinate)
            .build
            .value
      }
    }

    lazy val cost: Double = transportFromStopTo.coordinate.distanceTo(transportToStopFrom.coordinate) * costs.distanceByKm
  }

}
