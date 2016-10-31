package searching

import base.LazyLoggerSupport
import cats.data.{ Xor, XorT }
import mapdomain.graph.Coordinate
import mapdomain.publictransport.Stop
import model.{ BusPath, Path, PathDescription }
import provider.PublicTransportProviderSupport
import searching.SearchRoutingErrors.{ NoPathBetweenStops, SearchRoutingError }

import scala.concurrent.{ ExecutionContext, Future }

object PathBuilders {

  trait PathBuilder {
    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path]
  }

  case class TPPathBuilder(travelInfoId: Long, stopFrom: Stop, stopTo: Stop) extends PathBuilder with LazyLoggerSupport with PublicTransportProviderSupport {
    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
      Future[Xor[SearchRoutingError, Path]] {
        //      val travelInfo = publicTransportProvider.findTravelInfo(stopFrom.travelInfoId)
        val coordinates = publicTransportProvider.getPathBetweenStops(stopFrom, stopTo)
        Xor.Right(Path(coordinates, PathDescription(BusPath, "stop from address", "stop to address")))
      } recover {
        case exc: Throwable ⇒
          logger.error(s"An error occur trying to build the path between stops. $stopFrom, $stopTo", exc)
          Xor.Left(NoPathBetweenStops)
      }
    }
  }

  case class WalkPathBuilder(from: Coordinate, to: Coordinate) extends PathBuilder with WalkRouteSearcherSupport {
    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = {
      this.walkRouteSearcher.search(from, to)
    }
  }

}
