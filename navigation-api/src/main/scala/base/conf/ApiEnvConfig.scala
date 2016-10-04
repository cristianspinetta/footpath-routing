package base.conf

import base.LazyLoggerSupport
import com.typesafe.config.Config

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

trait ApiEnvConfig {
  lazy val configuration = ApiEnvConfig
}

object ApiEnvConfig extends EnvConfig with LazyLoggerSupport {

  val OSM = OSMConfiguration(envConfiguration.config.getConfig("osm"))
  val Ramp = RampConfiguration(envConfiguration.config.getConfig("ramp"))
  val HTTP = HTTPConfiguration(envConfiguration.config.getConfig("http"))
  val Graph = GraphConfiguration(envConfiguration.config.getConfig("graph"))

  logger.info(s"Configuration for env ${envConfiguration.currentEnvironment} loads OK")

}

case class OSMConfiguration(config: Config) {
  val sourceFilePath: String = config.getString("source-file")
}

case class RampConfiguration(config: Config) {
  val sourceFile2014Path: String = config.getString("source-file-2014")
  val sourceFile2011Path: String = config.getString("source-file-2011")
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
