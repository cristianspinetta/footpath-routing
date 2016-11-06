package searching.transports

import mapdomain.publictransport.{PublicTransportCombination, Stop}
import provider.PublicTransportProviderSupport
import searching.PathBuilders.PathBuilder
import utils.CollectionUtils

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

private[transports] object CombinationManager extends CombinationManager

private[transports] trait CombinationManager extends PublicTransportProviderSupport {
  import CombinationModel._

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
      .flatMap { case (transportFromId, combinationsBuilder) =>
        val combinations = combinationsBuilder.result()
        combinations
          .groupBy(_.transportToId)
          .flatMap {
            case (transportToId, partialCombinations@x :: xs) =>
              Combination(
                transportFromId = transportFromId,
                transportToId = transportToId,
                stopsFrom = partialCombinations.head.stopsFrom,
                linkedStops = partialCombinations.map(c => c.linkedStop)) :: Nil
            case _ => Nil
          }

      }
    CombinationContext(finalCombinations)
  }

  def reachableTransports(candidatesFrom: List[CandidateTransport], candidatesTo: List[CandidateTransport]): List[ReachableTransport] = (for {
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

  def canReachDestination(stopsFrom: List[Stop], stopsTo: List[Stop]): Boolean = {
    stopsFrom.minBy(_.sequence).sequence <= stopsTo.maxBy(_.sequence).sequence
  }
}

private[transports] object CombinationModel {

  case class CandidateTransport(travelInfoId: Long, stops: List[Stop])
  case class NearestCandidateTransports(from: List[CandidateTransport], to: List[CandidateTransport])

  case class ReachableTransport(travelInfoId: Long, stopsFrom: List[Stop], stopsTo: List[Stop])
  case class PartialRoute(travelInfoFromId: Long, stopFrom: Stop, pathBuilders: List[PathBuilder]) {
    lazy val cost: Double = pathBuilders.map(_.cost).sum
  }

  case class LinkedStops(transportFromStopTo: Stop, transportToStopFrom: Stop)
  case class Combination(transportFromId: Long, stopsFrom: List[Stop], transportToId: Long, linkedStops: List[LinkedStops])
  case class PartialCombination(transportFromId: Long, stopsFrom: List[Stop], transportToId: Long, linkedStop: LinkedStops)

  case class CombinationContext(combinations: List[Combination]) {
    lazy val byTransportFrom: Map[Long, List[Combination]] = combinations.groupBy(_.transportFromId).map(c => c._1 -> c._2)
    lazy val byTransportTo: Map[Long, List[Combination]] = combinations.groupBy(_.transportToId).map(c => c._1 -> c._2)
    lazy val transportsTo: List[CandidateTransport] = {
      val stopsWithTransportTo = for {
        (transportToId, combinations) <- byTransportTo.toList
        combination <- combinations
        linkedStop <- combination.linkedStops
      } yield (transportToId, linkedStop.transportToStopFrom)
      stopsWithTransportTo
        .groupBy(_._1)
        .map { case (transportToId, stops) => CandidateTransport(transportToId, CollectionUtils.removeDuplicated[Stop](stops.map(_._2), (s1, s2) => s1.id == s2.id)) }
        .toList
    }
  }
}

