package provider

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{ Path, Stop, TravelInfo }
import mapdomain.repository.publictransport.PublicTransportRepositorySupport
import spray.json._

trait PublicTransportProviderSupport {
  def publicTransportProvider = PublicTransportProvider
}

object PublicTransportProvider extends PublicTransportRepositorySupport {

  import module.Protocol._

  def findNearestStops(startPosition: Coordinate, radius: Double): List[Stop] = stopRepository.findNearestStops(startPosition, radius)

  def findTravelInfo(id: Long): TravelInfo = travelInfoRepository.find(id).get

  def getPathBetweenStops(stopFrom: Stop, stopTo: Stop): List[Coordinate] = {

    def findNextPaths(from: Stop, destination: Stop, accumulatedPaths: List[Path]): List[Path] = {
      if (from.id == destination.id) accumulatedPaths
      else {
        val nextStop: Option[Stop] = stopRepository.find(stopFrom.nextStopId.get)
        pathRepository.find(nextStop.get.pathId).get :: accumulatedPaths
      }
    }

    findNextPaths(stopFrom, stopTo, List.empty).reverse.flatMap(_.coordinates.parseJson.convertTo[List[Coordinate]])
  }
}


