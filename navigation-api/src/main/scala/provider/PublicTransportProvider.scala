package provider

import mapdomain.graph.Coordinate
import mapdomain.publictransport.Stop
import mapdomain.repository.publictransport.PublicTransportRepositorySupport

trait PublicTransportProviderSupport {
  def publicTransportProvider = PublicTransportProvider
}

object PublicTransportProvider extends PublicTransportRepositorySupport {

  def findNearestStops(startPosition: Coordinate, radius: Double): List[Stop] = stopRepository.findNearestStops(startPosition, radius)
}
