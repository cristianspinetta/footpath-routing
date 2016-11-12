package provider

import base.conf.ApiEnvConfig
import mapdomain.graph.Coordinate
import mapdomain.repository.sidewalk.RampRepositorySupport
import mapdomain.sidewalk.Ramp

trait RampProviderSupport {
  val rampProvider: RampProvider = RampProvider
}

trait RampProvider extends ApiEnvConfig with RampRepositorySupport {

  def findRamp(id: Long): Option[Ramp] = rampRepository.find(id)

  def findNearestRamps(coordinate: Coordinate, radius: Double, associated: Boolean): List[Ramp] = {
    if (associated) rampRepository.findNearestRampsAssociated(coordinate, radius)
    else rampRepository.findNearestRampsNotAssociated(coordinate, radius)
  }

}

object RampProvider extends RampProvider
