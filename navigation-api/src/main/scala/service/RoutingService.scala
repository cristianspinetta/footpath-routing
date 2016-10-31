package service

import base.conf.ApiEnvConfig
import base.{ Contexts, LazyLoggerSupport }
import cats.data.XorT
import mapdomain.graph._
import model.{ AccessibilityHeuristicType, HeuristicType, _ }
import provider.GraphSupport
import searching.SearchRoutingErrors._
import searching.RouteSearcherSupport

import scala.concurrent.{ ExecutionContext, Future }

trait RoutingServiceSupport {
  val routingService: RoutingService = RoutingService
}

trait RoutingService extends LazyLoggerSupport with ApiEnvConfig with RouteSearcherSupport {
  val walkRadius: Double = configuration.Routing.maximumWalkRadius
  implicit val routingExecutionContext: ExecutionContext = Contexts.routingExecutionContext

  def searchRoute(from: Coordinate, to: Coordinate, heuristicType: HeuristicType): XorT[Future, SearchRoutingError, List[Route]] = {
    routeSearcher.search(from, to, heuristicType)
  }
}

object RoutingService extends RoutingService
