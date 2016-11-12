package base

import snapshot.sidewalk.SidewalkVertexSnapshot
import snapshot.street.StreetVertexSnapshot
import snapshot.Snapshots
import snapshot.publictransport.StopSnapshot

object ApiSnapshots extends Snapshots {

  override val snapshots = Seq(StreetVertexSnapshot, SidewalkVertexSnapshot, StopSnapshot)

  override val version = "0.0.1" // FIXME get version from version.sbt

}
