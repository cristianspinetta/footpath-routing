package mapdomain.snapshot.street

import base.conf.EnvConfig
import mapdomain.repository.street.StreetVertexRepository
import mapdomain.street.{ StreetEdge, StreetVertex }
import snapshot.GroupedSnapshot

import scala.language.higherKinds

object StreetVertexSnapshot extends GroupedSnapshot[Long, StreetVertex[StreetEdge]] with EnvConfig {

  override val keyFunction: (StreetVertex[StreetEdge]) ⇒ Long = _.id

  override def cronExpression: String = envConfiguration.config.getString("snapshots.street-vertex.cron-expression")

  /**
   * Fetches data from source to build the snapshot
   */
  override def fetch: List[StreetVertex[StreetEdge]] = StreetVertexRepository.findAll
}
