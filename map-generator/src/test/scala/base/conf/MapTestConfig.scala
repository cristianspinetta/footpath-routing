package base.conf

import base.LazyLoggerSupport
import com.typesafe.config.{ Config, ConfigFactory }

trait MapTestConfig {
  lazy val configuration = TestConfig
}

object TestConfig extends LazyLoggerSupport {

  val defaultConfig = ConfigFactory.load()

  val OSM = OSMConfiguration(defaultConfig.getConfig("osm"))

  logger.info(s"Configuration for Map Generator Test loads OK")

}

case class OSMConfiguration(config: Config) {
  val sourceFilePath: String = config.getString("source-file")
  val otpFilePath: String = config.getString("otp-file")
}
