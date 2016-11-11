package base.conf

import java.util.concurrent.TimeUnit

import base.LazyLoggerSupport
import com.typesafe.config.Config

import scala.concurrent.duration.Duration

trait ApiEnvConfig {
  lazy val configuration = ApiEnvConfig
}

object ApiEnvConfig extends EnvConfig with LazyLoggerSupport {

  val OSM = OSMConfiguration(envConfiguration.config.getConfig("osm"))
  val Ramp = RampConfiguration(envConfiguration.config.getConfig("ramp"))
  val HTTP = HTTPConfiguration(envConfiguration.config.getConfig("http"))
  val Graph = GraphConfiguration(envConfiguration.config.getConfig("graph"))
  val Routing = RoutingConfiguration(envConfiguration.config.getConfig("routing"))

  logger.info(s"Configuration for env ${envConfiguration.currentEnvironment} loads OK")

}

case class OSMConfiguration(config: Config) {
  val sourceFilePath: String = config.getString("source-file")
}

case class RampConfiguration(config: Config) {
  val sourceFile2014Path: String = config.getString("source-file-2014")
  val sourceFile2011Path: String = config.getString("source-file-2011")
  val sourceFileRampAssociationPath: String = config.getString("ramps-association")
}

case class HTTPConfiguration(config: Config) {
  val interface: String = config.getString("interface")
  val port: Int = config.getInt("port")
}

case class GraphConfiguration(private val config: Config) {
  val javaDuration = config.getDuration("loading-timeout")
  val loadingTimeout = Duration(javaDuration.getSeconds, TimeUnit.SECONDS)
  val street = StreetGraphConf(config.getConfig("street"))
  val sidewalk = SidewalkGraphConf(config.getConfig("sidewalk"))
}

case class StreetGraphConf(private val config: Config) {
  val inMemory: Boolean = config.getBoolean("in-memory")
}
case class SidewalkGraphConf(private val config: Config) {
  val inMemory: Boolean = config.getBoolean("in-memory")
}

case class RoutingConfiguration(private val config: Config) {
  val maximumWalkRadius = config.getDouble("maximum-walk-radius")
  val heuristicCost = HeuristicCost(config.getConfig("heuristic-cost"))

  sealed case class HeuristicCost(private val config: Config) {
    val inaccessibleSidewalk = config.getInt("inaccessible-sidewalk")
    val inaccessibleRamp = config.getInt("inaccessible-ramp")
    val inaccessibleStop = config.getInt("inaccessible-stop")

    val stop = config.getInt("stop")
    val combination = config.getInt("combination")
    val distanceByKm = config.getInt("distance-by-km")
  }
}
