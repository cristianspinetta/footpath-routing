package snapshot.ramp

import base.conf.EnvConfig
import mapdomain.repository.sidewalk.RampRepository
import mapdomain.sidewalk.Ramp
import snapshot.GroupedSnapshot

object RampSnapshot extends GroupedSnapshot[Long, Ramp] with EnvConfig {

  override val keyFunction: (Ramp) ⇒ Long = _.id.get

  override def cronExpression: String = envConfiguration.config.getString("snapshots.ramp.cron-expression")

  /**
   * Fetches data from source to build the snapshot
   */
  override def fetch: List[Ramp] = RampRepository.findAll

}
