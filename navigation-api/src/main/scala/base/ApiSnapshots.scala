package base

import snapshot.sidewalk.SidewalkVertexSnapshot
import snapshot.street.StreetVertexSnapshot
import snapshot.Snapshots

object ApiSnapshots extends Snapshots {

  override val snapshots = Seq(StreetVertexSnapshot, SidewalkVertexSnapshot)

  override val version = "0.0.1" // FIXME get version from version.sbt

}
