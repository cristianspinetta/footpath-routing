package searching.transports

import base.{LazyLoggerSupport, MeterSupport}
import cats.data.{Xor, XorT}
import cats.implicits._
import mapdomain.graph._
import model._
import provider.{GraphSupport, PublicTransportProviderSupport}
import searching.PathBuilders._
import searching.SearchRoutingErrors.{NoStops, SearchRoutingError}
import searching.WalkRouteSearcherSupport

import scala.concurrent.{ExecutionContext, Future}

trait PublicTransportRouteSearcherSupport {
  protected val publicTransportRouteSearcher = PublicTransportRouteSearcher
}

private[transports] object PublicTransportRouteSearcher extends PublicTransportRouteSearcher

sealed trait PublicTransportRouteSearcher extends WalkRouteSearcherSupport
  with GraphSupport with PublicTransportProviderSupport with LazyLoggerSupport with MeterSupport {

  import base.XorTSugars._
  import searching.transports.CombinationManager._
  import CombinationModel._

  def search(from: Coordinate, to: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = withTimeLoggingAsync({
    searchNearestStops(from, to).flatMap(searchPathsByTPCombinations(from, to))
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

  protected def searchPathsByTPCombinations(from: Coordinate, to: Coordinate)(candidatePTs: NearestCandidateTransports)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = {
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
              val partialRoutes = createRoutes(to, candidatePaths).take(requiredPaths)
              Xor.Right(partialRoutes)
            case (paths, foundPaths) ⇒

              val directPartialRoutes = createRoutes(to, candidatePaths)

              val allFromCombinations = publicTransportProvider.getTPCombinationsByMultipleTravelInfoIds(candidatesFrom.map(_.travelInfoId))
              val combinationContext = crateCombinationContext(candidatesFrom, allFromCombinations)

              val newCandidateFrom: List[CandidateTransport] = combinationContext.transportsTo
              val xorCombinationsF = searchTransportRoute(newCandidateFrom, candidatesTo, attempt + 1, requiredPaths - foundPaths)
                .map { partialNextPaths: List[PartialRoute] ⇒
                  val sortedPartialNextPaths = partialNextPaths.sortBy(_.cost).take(requiredPaths - foundPaths)

                  for {
                    partialNextPath ← sortedPartialNextPaths
                  } yield {

                    val selectedCombinations: List[Combination] = combinationContext.byTransportTo(partialNextPath.travelInfoFromId)

                    // FIXME select the lowest cost one
                    val selectedCombination = selectedCombinations.find(c => c.linkedStops.exists(_.transportToStopFrom.id == partialNextPath.stopFrom.id)).get

                    // TODO: Build walk path from combination
                    // FIXME hacer una funcion que dado una lista de paradas desde y paradas hasta elija una buena parada de cada extremo para hacer el path
                    val pathBuilders = List(
                      TPPathBuilder(selectedCombination.transportFromId, selectedCombination.stopsFrom.head, selectedCombination.linkedStops.head.transportFromStopTo),
                      WalkPathBuilder(selectedCombination.linkedStops.head.transportFromStopTo.coordinate, partialNextPath.stopFrom.coordinate))
                    PartialRoute(selectedCombination.transportFromId, selectedCombination.stopsFrom.head, pathBuilders ::: partialNextPath.pathBuilders)
                  }
                }

              xorCombinationsF
                .map(combinationRoutes => combinationRoutes ::: directPartialRoutes)
                .recover { case _ =>
                  logger.info(s"Return from recursively searchTransportRoute via recovery with the direct partial routes. [Size = $foundPaths]")
                  directPartialRoutes
                }
          }
        case _ ⇒
          logger.info(s"No more stops for searching route via Transport Public. [Candidate Stops from = ${candidatesFrom.size}, Candidate Stops to = ${candidatesTo.size}]")
          Xor.Left(NoStops)
      }
    }, (timing: Long) => logger.info(s"Searching a route with ${attempt - 1} transport combinations took $timing ms."))

    val partialRoutes: Xor[SearchRoutingError, List[PartialRoute]] =
      searchTransportRoute(candidatePTs.from, candidatePTs.to, attempt = 1, requiredPaths = 10)
      .map(routes => routes.sortBy(_.cost))

    XorT(Future.successful(partialRoutes)).flatMap { partials ⇒
      partials.traverseU { partial ⇒
        val firstPath = WalkPathBuilder(from, partial.stopFrom.coordinate)
        (firstPath :: partial.pathBuilders)
          .traverseU(routeBuilder ⇒ routeBuilder.build)
          .map(paths ⇒ Route(paths))
      }
    }
  }

  //  protected def mergeStops(stops: List[Stop]): List[CandidateTransport] = {
  //    val result = new TrieMap[Long,  mutable.Builder[Stop, List[Stop]]]()
  //    stops foreach { stop => result getOrElseUpdate (stop.travelInfoId, List.newBuilder[Stop]) += stop }
  //    result map { case (travelInfoId, stopsBuilder) =>
  //      val uniqueStops: List[Stop] = removeDuplicated(stopsBuilder.result(), (s1, s2) => s1.id == s2.id)
  //      CandidateTransport(travelInfoId, uniqueStops)
  //    } toList
  //  }
  //
  //  protected def mergeTransports(candidateTransports: List[CandidateTransport]): List[CandidateTransport] = {
  //    mergeStops(candidateTransports.flatMap(_.stops))
  //  }
  def createRoutes(to: Coordinate, candidatePaths: List[ReachableTransport]): List[PartialRoute] = {
    candidatePaths map { candidate ⇒
      val stopTo = candidate.stopsTo.minBy(stop ⇒ stop.coordinate.distanceTo(to))
      val pathBuilders = List(TPPathBuilder(candidate.travelInfoId, candidate.stopsFrom.head, stopTo), WalkPathBuilder(stopTo.coordinate, to))
      PartialRoute(candidate.travelInfoId, candidate.stopsFrom.head, pathBuilders)
    }
  }
}
