package searching

import base.{ LazyLoggerSupport, LogicError, MeterSupport }
import cats.data.{ Xor, XorT }
import cats.implicits._
import mapdomain.graph._
import mapdomain.publictransport.{ PublicTransportCombination, Stop }
import model._
import provider.{ GraphSupport, PublicTransportProviderSupport }
import SearchRoutingErrors._

import scala.concurrent.{ ExecutionContext, Future }
import searching.PathBuilders._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

trait PublicTransportRouteSearcherSupport {
  protected val publicTransportRouteSearcher = PublicTransportRouteSearcher
}

object PublicTransportRouteSearcher extends PublicTransportRouteSearcher

sealed trait PublicTransportRouteSearcher extends WalkRouteSearcherSupport
    with GraphSupport with PublicTransportProviderSupport with LazyLoggerSupport with MeterSupport {

  import base.XorTSugars._

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

    def candidatePathByTravelInfo(candidatesFrom: List[CandidateTransport], candidatesTo: List[CandidateTransport]): List[ReachableTransport] = (for {
      from ← candidatesFrom
      to ← candidatesTo if to.travelInfoId == from.travelInfoId
    } yield {
      val stopsFrom: List[Stop] = from.stops
      val stopsTo: List[Stop] = to.stops
      if (canReachDestination(stopsFrom, stopsTo))
        List(ReachableTransport(to.travelInfoId, stopsFrom, stopsTo))
      else
        List.empty[ReachableTransport]
    }).flatten

    def searchPT(candidatesFrom: List[CandidateTransport], candidatesTo: List[CandidateTransport], attempt: Long): Xor[SearchRoutingError, List[PartialRoute]] = withTimeLogging({
      assert(attempt < 3, s"Reached the maximum attempt to search combinations [attempt = $attempt]")
      logger.info(s"Searching a route picking $attempt Public Transport(s) to reach the destination. There are ${candidatesFrom.size} candidate Transports on the source and ${candidatesTo.size} candidate Transports on the destination.")
      logger.debug(s"Search a path with: [candidatesFrom = ${candidatesFrom.map(_.travelInfoId)}, candidatesTo = ${candidatesTo.map(_.travelInfoId)}]")

      (candidatesFrom, candidatesTo) match {
        case (x :: xs, y :: ys) ⇒
          // is there a direct Public Transport?
          val candidatePaths: List[ReachableTransport] = candidatePathByTravelInfo(candidatesFrom, candidatesTo)

          candidatePaths match {
            case Nil ⇒

              val allFromCombinations = publicTransportProvider.getTPCombinationsByMultipleTravelInfoIds(candidatesFrom.map(_.travelInfoId))
              val combinationContext = crateCombinationContext(candidatesFrom, allFromCombinations)

              val newCandidateFrom: List[CandidateTransport] = combinationContext.transportsTo
              searchPT(newCandidateFrom, candidatesTo, attempt + 1).map { partialNextPaths: List[PartialRoute] ⇒
                for {
                  partialNextPath ← partialNextPaths.take(10) // FIXME sort by cost
                } yield {

                  val selectedCombinations: List[Combination] = combinationContext.byTransportTo(partialNextPath.travelInfoFromId)

                  // FIXME select the lowest cost one
                  val selectedCombination = selectedCombinations.find(c ⇒ c.linkedStops.exists(_.transportToStopFrom.id == partialNextPath.stopFrom.id)).get

                  // TODO: Build walk path from combination
                  // FIXME hacer una funcion que dado una lista de paradas desde y paradas hasta elija una buena parada de cada extremo para hacer el path
                  val pathBuilders = List(
                    TPPathBuilder(selectedCombination.transportFromId, selectedCombination.stopsFrom.head, selectedCombination.linkedStops.head.transportFromStopTo),
                    WalkPathBuilder(selectedCombination.linkedStops.head.transportFromStopTo.coordinate, partialNextPath.stopFrom.coordinate))
                  PartialRoute(selectedCombination.transportFromId, selectedCombination.stopsFrom.head, pathBuilders ::: partialNextPath.pathBuilders)
                }
              }
            case _ ⇒
              Xor.Right(candidatePaths map { candidate ⇒
                val stopTo = candidate.stopsTo.minBy(stop ⇒ stop.coordinate.distanceTo(to))
                val pathBuilders = List(TPPathBuilder(candidate.travelInfoId, candidate.stopsFrom.head, stopTo), WalkPathBuilder(stopTo.coordinate, to))
                PartialRoute(candidate.travelInfoId, candidate.stopsFrom.head, pathBuilders)
              })
          }
        case _ ⇒
          logger.info(s"No more stops for searching route via Transport Public. [Candidate Stops from = ${candidatesFrom.size}, Candidate Stops to = ${candidatesTo.size}]")
          Xor.Left(NoStops)
      }
    }, (timing: Long) ⇒ logger.info(s"Searching a route with ${attempt - 1} transport combinations took $timing ms."))

    val partialRoutes: Xor[SearchRoutingError, List[PartialRoute]] = searchPT(candidatePTs.from, candidatePTs.to, attempt = 1)

    XorT(Future.successful(partialRoutes)).flatMap { partials ⇒
      partials.traverseU { partial ⇒
        val firstPath = WalkPathBuilder(from, partial.stopFrom.coordinate)
        (firstPath :: partial.pathBuilders)
          .traverseU(routeBuilder ⇒ routeBuilder.build)
          .map(paths ⇒ Route(paths))
      }
    }
  }

  protected def canReachDestination(stopsFrom: List[Stop], stopsTo: List[Stop]): Boolean = {
    stopsFrom.minBy(_.sequence).sequence <= stopsTo.maxBy(_.sequence).sequence
  }

  sealed case class CandidateTransport(travelInfoId: Long, stops: List[Stop])
  sealed case class NearestCandidateTransports(from: List[CandidateTransport], to: List[CandidateTransport])

  sealed case class ReachableTransport(travelInfoId: Long, stopsFrom: List[Stop], stopsTo: List[Stop])
  sealed case class PartialRoute(travelInfoFromId: Long, stopFrom: Stop, pathBuilders: List[PathBuilder])

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

  protected def removeDuplicated[A](elems: List[A], customEquals: (A, A) ⇒ Boolean): List[A] = {
    elems.foldRight(List.empty[A]) {
      (curr, unique) ⇒
        {
          if (!unique.exists(customEquals(curr, _)))
            curr +: unique
          else
            unique
        }
    }
  }

  case class LinkedStops(transportFromStopTo: Stop, transportToStopFrom: Stop)
  case class Combination(transportFromId: Long, stopsFrom: List[Stop], transportToId: Long, linkedStops: List[LinkedStops])
  case class PartialCombination(transportFromId: Long, stopsFrom: List[Stop], transportToId: Long, linkedStop: LinkedStops)
  case class CombinationContext(combinations: List[Combination]) {
    lazy val byTransportFrom: Map[Long, List[Combination]] = combinations.groupBy(_.transportFromId).map(c ⇒ c._1 -> c._2)
    lazy val byTransportTo: Map[Long, List[Combination]] = combinations.groupBy(_.transportToId).map(c ⇒ c._1 -> c._2)
    lazy val transportsTo: List[CandidateTransport] = {
      val stopsWithTransportTo = for {
        (transportToId, combinations) ← byTransportTo.toList
        combination ← combinations
        linkedStop ← combination.linkedStops
      } yield (transportToId, linkedStop.transportToStopFrom)
      stopsWithTransportTo
        .groupBy(_._1)
        .map { case (transportToId, stops) ⇒ CandidateTransport(transportToId, removeDuplicated[Stop](stops.map(_._2), (s1, s2) ⇒ s1.id == s2.id)) }
        .toList
    }
  }

  def crateCombinationContext(candidatesFrom: List[CandidateTransport], allCombination: List[PublicTransportCombination]): CombinationContext = {
    val candidatesFromMap: Map[Long, CandidateTransport] = candidatesFrom.map(c ⇒ c.travelInfoId -> c).toMap
    val result = new TrieMap[Long, mutable.Builder[PartialCombination, List[PartialCombination]]]()

    def addCombination(candidateTFrom: CandidateTransport, transportFromStopTo: Stop, transportToStopFrom: Stop, combination: PublicTransportCombination): Unit = {
      val key = candidateTFrom.travelInfoId

      val builder = result.getOrElseUpdate(key, List.newBuilder[PartialCombination])

      builder += PartialCombination(
        transportFromId = candidateTFrom.travelInfoId,
        stopsFrom = candidateTFrom.stops,
        transportToId = transportToStopFrom.travelInfoId,
        linkedStop = LinkedStops(transportFromStopTo = transportFromStopTo, transportToStopFrom = transportToStopFrom))
    }

    allCombination.foreach { combination ⇒
      candidatesFromMap
        .get(combination.fromTravelInfoId).orElse(candidatesFromMap.get(combination.toTravelInfoId))
        .foreach(tiFrom ⇒
          if (tiFrom.travelInfoId == combination.fromTravelInfoId
            && canReachDestination(tiFrom.stops, List(publicTransportProvider.findStop(combination.fromStopId)))) {

            val transportFromStopTo = publicTransportProvider.findStop(combination.fromStopId)
            val transportToStopFrom = publicTransportProvider.findStop(combination.toStopId)
            addCombination(tiFrom, transportFromStopTo, transportToStopFrom, combination)

          } else if (tiFrom.travelInfoId == combination.toTravelInfoId
            && canReachDestination(tiFrom.stops, List(publicTransportProvider.findStop(combination.toStopId)))) {

            val transportFromStopTo = publicTransportProvider.findStop(combination.toStopId)
            val transportToStopFrom = publicTransportProvider.findStop(combination.fromStopId)
            addCombination(tiFrom, transportFromStopTo, transportToStopFrom, combination)
          })
    }

    // Flat Combinations
    val finalCombinations: List[Combination] = result
      .toList
      .flatMap {
        case (transportFromId, combinationsBuilder) ⇒
          val combinations = combinationsBuilder.result()
          combinations
            .groupBy(_.transportToId)
            .flatMap {
              case (transportToId, partialCombinations @ x :: xs) ⇒
                Combination(
                  transportFromId = transportFromId,
                  transportToId = transportToId,
                  stopsFrom = partialCombinations.head.stopsFrom,
                  linkedStops = partialCombinations.map(c ⇒ c.linkedStop)) :: Nil
              case _ ⇒ Nil
            }

      }
    CombinationContext(finalCombinations)
  }

}
