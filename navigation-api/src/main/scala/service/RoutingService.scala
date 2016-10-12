package service

import base.conf.ApiEnvConfig
import base.{ Contexts, LazyLoggerSupport }
import cats.data.XorT
import mapdomain.graph._
import model._
import provider.{ GraphSupport, PublicTransportProviderSupport }
import searching.{ PublicTransportRouteSearcher, SearchRoutingError, WalkRouteSearcher }
import cats.implicits._

import scala.concurrent.{ ExecutionContext, Future }

trait RoutingService extends GraphSupport with LazyLoggerSupport with ApiEnvConfig with PublicTransportProviderSupport {
  val walkRadius: Double = configuration.Routing.maximumWalkRadius
  implicit val routingExecutionContext: ExecutionContext = Contexts.routingExecutionContext

  def searchRoute(from: Coordinate, to: Coordinate): XorT[Future, SearchRoutingError, List[Route]] = {
    logger.info(s"Init Search from $from to $to")
    if (from.distanceTo(to) <= walkRadius)
      WalkRouteSearcher.search(from, to).map(path ⇒ List(Route(List(path))))
    else
      PublicTransportRouteSearcher.search(from, to)
  }
}

object RoutingService extends RoutingService
