package conf

import java.io.File

import base.LazyLoggerSupport
import com.typesafe.config.{ Config, ConfigFactory }

trait EnvConfig {
  lazy val envConfiguration = EnvConfiguration
}

object EnvConfiguration extends LazyLoggerSupport {

  val config = loadConfig()
  val currentEnvironment: String = Option(System.getProperty("environment")).get//OrElse(config.getString("environment"))

  private def loadConfig(): Config = {

    val defaultConfig = ConfigFactory.load()

    val overrideFile = new File(Option(System.getProperty("environmentOverride")).getOrElse("environment-override.conf"))
    val envConfiguration = ConfigFactory.parseFile(overrideFile) /*.withFallback(defaultConfig.getConfig(environment))*/
      .withFallback(defaultConfig)

    envConfiguration.resolve()
  }
}
