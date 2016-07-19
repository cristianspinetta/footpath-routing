package conf

import base.LazyLoggerSupport
import com.typesafe.config.Config

trait ApiEnvConfig {
  lazy val configuration = ApiEnvConfig
}

object ApiEnvConfig extends EnvConfig with LazyLoggerSupport {

  val OSM = OSMConfiguration(envConfiguration.config.getConfig("osm"))
  val Ramp = RampConfiguration(envConfiguration.config.getConfig("ramp"))
  val HTTP = HTTPConfiguration(envConfiguration.config.getConfig("http"))

}

case class OSMConfiguration(config: Config) {
  val sourceFilePath: String = config.getString("source-file")
  val otpFilePath: String = config.getString("otp-file")
}

case class RampConfiguration(config: Config) {
  val sourceFile2014Path: String = config.getString("source-file-2014")
  val sourceFile2011Path: String = config.getString("source-file-2011")
}

case class HTTPConfiguration(config: Config) {
  val interface: String = config.getString("interface")
  val port: Int = config.getInt("port")
}
