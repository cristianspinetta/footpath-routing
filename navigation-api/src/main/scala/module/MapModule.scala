package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import model.{ Sidewalk, Street }

import scala.util.Try

trait MapModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def streets: Try[List[Street]] = Try(graphProvider.streets.map(Street.apply))
  def sidewalks: Try[Set[Sidewalk]] = Try(graphProvider.sidewalks.map(Sidewalk.apply))

}

object MapModule extends MapModule
