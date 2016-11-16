package base

import base.conf.ApiEnvConfig
import snapshot.publictransport.StopSnapshot
import snapshot.ramp.RampSnapshot
import snapshot.sidewalk.SidewalkVertexSnapshot
import snapshot.street.StreetVertexSnapshot
import snapshot.{BaseSnapshot, Snapshots}

import scala.concurrent.Future
import scala.util.Try

object ApiSnapshots extends Snapshots with ApiEnvConfig with LazyLoggerSupport with MeterSupport {

  protected def parallelLoaders: Int = configuration.Snapshots.parallelLoading

  val snapshotByName = Map(
    "street-vertex" -> StreetVertexSnapshot,
    "sidewalk-vertex" -> SidewalkVertexSnapshot,
    "stop" -> StopSnapshot,
    "ramp" -> RampSnapshot)

  override val snapshots: Seq[BaseSnapshot[_, _]] = snapshotByName.values.toSeq

  override val version = "0.0.1" // FIXME get version from version.sbt

  override def initialize(): Unit = withTimeLogging({
    super.initialize()
  }, (timing: Long) => logger.info(s"Initialize all snapshots in synchronous fashion took $timing ms."))

  override def initializeAsync(): Future[List[Unit]] = withTimeLoggingAsync({
    super.initializeAsync()
  }, (timing: Long) => logger.info(s"Initialize all snapshots in asynchronous fashion took $timing ms."))


  override def reload(): Try[Unit] = withTimeLogging({
    super.reload()
  }, (timing: Long) => logger.info(s"Reload all snapshots in synchronous fashion took $timing ms."))

  override def reloadParallel(): Future[List[Unit]] = withTimeLoggingAsync({
    super.reloadParallel()
  }, (timing: Long) => logger.info(s"Reload all snapshots in asynchronous fashion took $timing ms."))
}
