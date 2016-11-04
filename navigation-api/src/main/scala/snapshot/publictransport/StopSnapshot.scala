package snapshot.publictransport

import base.conf.EnvConfig
import mapdomain.publictransport.Stop
import mapdomain.repository.publictransport.StopRepository
import snapshot.GroupedSnapshot

object StopSnapshot extends GroupedSnapshot[Long, Stop] with EnvConfig {

  override val keyFunction: (Stop) ⇒ Long = _.id

  override def cronExpression: String = envConfiguration.config.getString("snapshots.stop.cron-expression")

  /**
    * Fetches data from source to build the snapshot
    */
  override def fetch: List[Stop] = StopRepository.findAll

}
