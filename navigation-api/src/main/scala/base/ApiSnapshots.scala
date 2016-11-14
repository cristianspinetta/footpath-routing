package base

import mapdomain.publictransport.Stop
import mapdomain.sidewalk.{ Ramp, SidewalkVertex }
import mapdomain.street.{ StreetEdge, StreetVertex }
import snapshot.sidewalk.SidewalkVertexSnapshot
import snapshot.street.StreetVertexSnapshot
import snapshot.{ BaseSnapshot, GroupedSnapshot, Snapshots }
import snapshot.publictransport.StopSnapshot
import snapshot.ramp.RampSnapshot

object ApiSnapshots extends Snapshots {

  val snapshotByName = Map(
    "street-vertex" -> StreetVertexSnapshot,
    "sidewalk-vertex" -> SidewalkVertexSnapshot,
    "stop" -> StopSnapshot,
    "ramp" -> RampSnapshot)

  override val snapshots: Seq[BaseSnapshot[_, _]] = snapshotByName.values.toSeq

  override val version = "0.0.1" // FIXME get version from version.sbt
}
