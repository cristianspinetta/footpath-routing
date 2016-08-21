package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import model.Street

import scala.util.Try

trait MapModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def streets: Try[List[Street]] = Try(graphProvider.streets.map(Street.apply))

}

object MapModule extends MapModule
