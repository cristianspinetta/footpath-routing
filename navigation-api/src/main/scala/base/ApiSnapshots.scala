package base

import snapshot.sidewalk.SidewalkVertexSnapshot
import snapshot.street.StreetVertexSnapshot
import snapshot.Snapshots
import snapshot.publictransport.StopSnapshot

object ApiSnapshots extends Snapshots {

  val snapshotByName = Map(
    "StreetVertex" -> StreetVertexSnapshot,
    "SidewalkVertex" -> SidewalkVertexSnapshot,
    "Stop" -> StopSnapshot)

  override val snapshots = Seq(StreetVertexSnapshot, SidewalkVertexSnapshot, StopSnapshot)

  override val version = "0.0.1" // FIXME get version from version.sbt
}
