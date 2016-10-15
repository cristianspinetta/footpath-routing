package service

import base.conf.ApiEnvConfig
import base.{ Contexts, LazyLoggerSupport }
import cats.data.XorT
import mapdomain.graph._
import model._
import provider.GraphSupport
import searching.SearchRoutingErrors._
import searching.RouteSearcherSupport

import scala.concurrent.{ ExecutionContext, Future }

trait RoutingService extends LazyLoggerSupport with ApiEnvConfig with RouteSearcherSupport {
  val walkRadius: Double = configuration.Routing.maximumWalkRadius
  implicit val routingExecutionContext: ExecutionContext = Contexts.routingExecutionContext

  def searchRoute(from: Coordinate, to: Coordinate): XorT[Future, SearchRoutingError, List[Route]] = {
    routeSearcher.search(from, to)
  }
}

object RoutingService extends RoutingService
