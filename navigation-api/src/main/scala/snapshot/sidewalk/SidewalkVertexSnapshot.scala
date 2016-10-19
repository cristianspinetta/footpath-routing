package snapshot.sidewalk

import base.conf.EnvConfig
import mapdomain.repository.sidewalk.SidewalkVertexRepository
import mapdomain.sidewalk.SidewalkVertex
import snapshot.GroupedSnapshot

object SidewalkVertexSnapshot extends GroupedSnapshot[Long, SidewalkVertex] with EnvConfig {

  override val keyFunction: (SidewalkVertex) ⇒ Long = _.id

  override def cronExpression: String = envConfiguration.config.getString("snapshots.street-vertex.cron-expression")

  /**
   * Fetches data from source to build the snapshot
   */
  override def fetch: List[SidewalkVertex] = SidewalkVertexRepository.findAll
}
