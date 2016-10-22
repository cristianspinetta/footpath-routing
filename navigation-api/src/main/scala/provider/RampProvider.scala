package provider

import base.conf.ApiEnvConfig
import mapdomain.repository.sidewalk.RampRepositorySupport
import mapdomain.sidewalk.Ramp

trait RampProviderSupport {
  val rampProvider: RampProvider = RampProvider
}

trait RampProvider extends ApiEnvConfig with RampRepositorySupport {

  def findRamp(id: Long): Option[Ramp] = rampRepository.find(id)

}

object RampProvider extends RampProvider
