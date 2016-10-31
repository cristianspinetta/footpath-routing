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

    def getCandidateCombinations(candidatesFromMap: Map[Long, CandidateTransport], ptCombinations: List[PublicTransportCombination]): List[CandidateCombination] = ptCombinations.flatMap { combination ⇒
      candidatesFromMap
        .get(combination.fromTravelInfoId).orElse(candidatesFromMap.get(combination.toTravelInfoId))
        .map(tiFrom ⇒
          if (tiFrom.travelInfoId == combination.fromTravelInfoId
            && canReachDestination(tiFrom.stops, List(publicTransportProvider.findStop(combination.fromStopId)))) {
            val stopTo = publicTransportProvider.findStop(combination.fromStopId)
            List(CandidateCombination(TravelInfoPathContext(tiFrom.travelInfoId, tiFrom.stops, stopTo), combination))
          } else if (tiFrom.travelInfoId == combination.toTravelInfoId
            && canReachDestination(tiFrom.stops, List(publicTransportProvider.findStop(combination.toStopId)))) {
            val stopTo = publicTransportProvider.findStop(combination.toStopId)
            List(CandidateCombination(TravelInfoPathContext(tiFrom.travelInfoId, tiFrom.stops, stopTo), combination))
          } else Nil)
        .toList.flatten

    }

    def searchPT(candidatesFrom: List[CandidateTransport], candidatesTo: List[CandidateTransport], attempt: Long): Xor[SearchRoutingError, List[PartialRoute]] = {
      assert(attempt < 3, s"Reached the maximum attempt to search combinations [attempt = $attempt]")
      logger.info(s"Searching a route taking $attempt Public Transport to reach the destination. There are ${candidatesFrom.size} candidate PT in the source and ${candidatesTo.size} candidate PT in the destination.")

      (candidatesFrom, candidatesTo) match {
        case (x :: xs, y :: ys) ⇒
          // is there a direct Public Transport?
          val candidatePaths: List[ReachableTransport] = candidatePathByTravelInfo(candidatesFrom, candidatesTo)

          candidatePaths match {
            case Nil ⇒
              val allFromCombinations = publicTransportProvider.getTPCombinationsByMultipleTravelInfoIds(candidatesFrom.map(_.travelInfoId))
              val candidateCombinations = getCandidateCombinations(candidatesFrom.map(c ⇒ c.travelInfoId -> c).toMap, allFromCombinations)

              val candidateCombinationsByToTI = candidateCombinations.map(cc ⇒ cc.toTravelInfoId -> cc) toMap

              val candidateFrom: List[CandidateTransport] = candidateCombinations.map(cc ⇒ CandidateTransport(cc.toTravelInfoId, List(cc.toStop)))
              searchPT(candidateFrom, candidatesTo, attempt + 1).map { partialNextPaths: List[PartialRoute] ⇒
                for {
                  partialNextPath ← partialNextPaths
                } yield {
                  val comb: CandidateCombination = candidateCombinationsByToTI(partialNextPath.travelInfoFromId)
                  // TODO: Build walk path from combination
                  val pathBuilders = List(TPPathBuilder(comb.fromTravelInfoId, comb.fromStop, comb.toStop), WalkPathBuilder(comb.toStop.coordinate, partialNextPath.stopFrom.coordinate))
                  PartialRoute(comb.fromTravelInfoId, comb.fromStop, pathBuilders)
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
    }

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
    stopsFrom.maxBy(_.sequence).sequence <= stopsTo.minBy(_.sequence).sequence
  }

  sealed case class CandidateTransport(travelInfoId: Long, stops: List[Stop])
  sealed case class NearestCandidateTransports(from: List[CandidateTransport], to: List[CandidateTransport])

  sealed case class TravelInfoPathContext(travelInfoId: Long, stopsFrom: List[Stop], stopTo: Stop)
  sealed case class CandidateCombination(travelInfoPathFrom: TravelInfoPathContext, combination: PublicTransportCombination) extends PublicTransportProviderSupport {

    lazy val fromTravelInfoId: Long = travelInfoPathFrom.travelInfoId
    lazy val toTravelInfoId: Long = if (combination.fromTravelInfoId == fromTravelInfoId) combination.toTravelInfoId else combination.fromTravelInfoId

    lazy val fromStop: Stop = travelInfoPathFrom.stopTo
    lazy val toStop: Stop =
      if (combination.fromTravelInfoId == fromTravelInfoId)
        publicTransportProvider.findStop(combination.toStopId)
      else
        publicTransportProvider.findStop(combination.fromStopId)
  }

  sealed case class ReachableTransport(travelInfoId: Long, stopsFrom: List[Stop], stopsTo: List[Stop])
  sealed case class PartialRoute(travelInfoFromId: Long, stopFrom: Stop, pathBuilders: List[PathBuilder])
}
