package searching.transports

import base.{ LazyLoggerSupport, MeterSupport }
import cats.data.{ Xor, XorT }
import cats.implicits._
import mapdomain.graph._
import mapdomain.publictransport.Stop
import model._
import provider.{ GraphSupport, PublicTransportProviderSupport }
import searching.PathBuilders._
import searching.SearchRoutingErrors.{ NoStops, SearchRoutingError }
import searching.walk.WalkRouteSearcherSupport

import scala.concurrent.{ ExecutionContext, Future }

trait PublicTransportRouteSearcherSupport {
  protected val publicTransportRouteSearcher = PublicTransportRouteSearcher
}

private[transports] object PublicTransportRouteSearcher extends PublicTransportRouteSearcher

sealed trait PublicTransportRouteSearcher extends WalkRouteSearcherSupport
    with GraphSupport with PublicTransportProviderSupport with LazyLoggerSupport with MeterSupport {

  import CombinationModel._
  import base.XorTSugars._
  import searching.transports.CombinationManager._

  def search(from: Coordinate, to: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = withTimeLoggingAsync({
    searchNearestStops(from, to).flatMap(searchPathsByCombinations(from, to))
  }, (time: Long) ⇒ logger.info(s"Execute Search route for Public Transport took $time ms."))

  protected def searchNearestStops(coordinateFrom: Coordinate, coordinateTo: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, NearestCandidateTransports] = XorT {
    logger.info(s"Searching nearest stops...")
    val nearestStopsFromFut = Future(publicTransportProvider.findStopsByRadiusAndLine(coordinateFrom, Some(walkRadius)))
    val nearestStopsToFut = Future(publicTransportProvider.findStopsByRadiusAndLine(coordinateTo, Some(walkRadius)))
    for {
      nearestStopsFrom ← nearestStopsFromFut
      nearestStopsTo ← nearestStopsToFut
    } yield {
      if (nearestStopsFrom.nonEmpty && nearestStopsTo.nonEmpty) {

        val candidatesPTFrom = nearestStopsFrom.groupBy(_.travelInfoId).map(r ⇒ CandidateTransport(r._1, r._2))
        val candidatesPTTo = nearestStopsTo.groupBy(_.travelInfoId).map(r ⇒ CandidateTransport(r._1, r._2))
        Xor.Right(NearestCandidateTransports(candidatesPTFrom toList, candidatesPTTo toList))
      } else
        Xor.Left(NoStops)
    }
  }

  protected def searchPathsByCombinations(from: Coordinate, to: Coordinate)(candidatePTs: NearestCandidateTransports)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = {
    logger.info("Searching paths between stops...")

    def searchTransportRoute(candidatesFrom: List[CandidateTransport], candidatesTo: List[CandidateTransport], attempt: Long, requiredPaths: Int): Xor[SearchRoutingError, List[PartialRoute]] = withTimeLogging({
      assert(attempt < 3, s"Reached the maximum attempt to search combinations [attempt = $attempt]")
      logger.info(s"Searching a route picking $attempt Public Transport(s) to reach the destination. There are ${candidatesFrom.size} candidate Transports on the source and ${candidatesTo.size} candidate Transports on the destination.")
      logger.debug(s"Search a path with: [candidatesFrom = ${candidatesFrom.map(_.travelInfoId)}, candidatesTo = ${candidatesTo.map(_.travelInfoId)}]")

      (candidatesFrom, candidatesTo) match {
        case (x :: xs, y :: ys) ⇒
          // is there a direct Public Transport?
          val candidatePaths: List[ReachableTransport] = reachableTransports(candidatesFrom, candidatesTo)

          (candidatePaths, candidatePaths.size) match {
            case (paths, foundPaths) if foundPaths >= requiredPaths ⇒
              val partialRoutes = createRoutes(to, candidatePaths).sortBy(_.cost).take(requiredPaths)
              Xor.Right(partialRoutes)
            case (paths, foundPaths) ⇒

              val directPartialRoutes = createRoutes(to, candidatePaths)

              val allFromCombinations = publicTransportProvider.getCombinationsByMultipleTravelInfoIds(candidatesFrom.map(_.travelInfoId), excludedRadius = List((from, walkRadius), (to, walkRadius)))
              val combinationContext = crateCombinationContext(candidatesFrom, allFromCombinations)

              val newCandidateFrom: List[CandidateTransport] = combinationContext.transportsTo

              val xorCombinationsF = searchTransportRoute(newCandidateFrom, candidatesTo, attempt + 1, requiredPaths - foundPaths)
                .map { partialNextPaths: List[PartialRoute] ⇒

                  partialNextPaths
                    .sortBy(_.cost)
                    .take(requiredPaths - foundPaths)
                    .map { partialNextPath ⇒
                      val (selectedCombination, linkedStop) = (for {
                        combination ← combinationContext.byTransportTo(partialNextPath.travelInfoFromId)
                        linkedStop ← combination.linkedStops if linkedStop.transportToStopFrom.id == partialNextPath.stopFrom.id
                      } yield (combination, linkedStop)).head

                      val transportFromId = selectedCombination.transportFromId
                      val transportFromStopTo = linkedStop.transportFromStopTo
                      val transportFromStopFrom = selectedCombination.stopsFrom.find(_.sequence < transportFromStopTo.sequence).get

                      // FIXME improve the selection of stop from and stop to
                      val pathBuilders = List(
                        TransportPathBuilder(transportFromId, transportFromStopFrom, transportFromStopTo),
                        WalkPathCombinationBuilder(linkedStop.transportFromStopTo, linkedStop.transportToStopFrom))

                      PartialRoute(transportFromId, transportFromStopFrom, pathBuilders ::: partialNextPath.pathBuilders)
                    }
                }

              xorCombinationsF
                .map(combinationRoutes ⇒ combinationRoutes ::: directPartialRoutes)
                .recover {
                  case _ ⇒
                    logger.info(s"Return from recursively searchTransportRoute via recovery with the direct partial routes. [Size = $foundPaths]")
                    directPartialRoutes
                }
          }
        case _ ⇒
          logger.info(s"No more stops for searching route via Transport Public. [Candidate Stops from = ${candidatesFrom.size}, Candidate Stops to = ${candidatesTo.size}]")
          Xor.Left(NoStops)
      }
    }, (timing: Long) ⇒ logger.info(s"Searching a route with ${attempt - 1} transport combinations took $timing ms."))

    val partialRoutes: Xor[SearchRoutingError, List[PartialRoute]] =
      searchTransportRoute(candidatePTs.from, candidatePTs.to, attempt = 1, requiredPaths = 10)
        .map(routes ⇒ routes.sortBy(_.cost))

    XorT(Future.successful(partialRoutes)).flatMap { partials ⇒
      partials.traverseU { partial ⇒
        val firstPath = WalkPathBuilder(from, partial.stopFrom.coordinate)
        (firstPath :: partial.pathBuilders)
          .traverseU(routeBuilder ⇒ routeBuilder.build)
          .map(paths ⇒ Route(paths))
      }
    }
  }

  def createRoutes(to: Coordinate, candidatePaths: List[ReachableTransport]): List[PartialRoute] = {
    candidatePaths map { candidate ⇒
      val stopTo = candidate.stopsTo.minBy(stop ⇒ stop.coordinate.distanceTo(to))
      val stopFrom: Stop = candidate.stopsFrom.find(_.sequence < stopTo.sequence).getOrElse(candidate.stopsFrom.head)
      val pathBuilders = List(TransportPathBuilder(candidate.travelInfoId, stopFrom, stopTo), WalkPathBuilder(stopTo.coordinate, to))
      PartialRoute(candidate.travelInfoId, stopFrom, pathBuilders)
    }
  }
}
