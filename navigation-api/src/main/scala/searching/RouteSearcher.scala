package searching

import base.LazyLoggerSupport
import cats.data.XorT
import cats.implicits._
import mapdomain.graph._
import model.Route

import scala.concurrent.{ ExecutionContext, Future }

trait RouteSearcherSupport {
  protected val routeSearcher = RouteSearcher
}

object RouteSearcher extends RouteSearcher

sealed trait RouteSearcher extends LazyLoggerSupport with WalkRouteSearcherSupport with PublicTransportRouteSearcherSupport {
  import SearchRoutingErrors._

  def search(from: Coordinate, to: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = {
    logger.info(s"Init Search from $from to $to")
    if (from.distanceTo(to) <= walkRadius)
      walkRouteSearcher.search(from, to).map(path ⇒ List(Route(List(path))))
    else
      publicTransportRouteSearcher.search(from, to)
  }
}

object SearchRoutingErrors {
  sealed trait SearchRoutingError
  case object NoStops extends SearchRoutingError
  case object NoPath extends SearchRoutingError
  case object NoPathBetweenStops extends SearchRoutingError
}
