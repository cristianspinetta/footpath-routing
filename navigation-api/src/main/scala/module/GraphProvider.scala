package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.sidewalk._
import mapdomain.street.{ LazyStreetGraphContainer, StreetGraphContainer }
import mapgenerator.source.features.{ RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV }

trait GraphSupport {
  val graphProvider = GraphProvider
}

object GraphProvider extends LazyLoggerSupport with ApiEnvConfig {

  private lazy val rampPath2014: String = getClass.getResource(configuration.Ramp.sourceFile2014Path).getPath
  private lazy val rampPath2011: String = getClass.getResource(configuration.Ramp.sourceFile2011Path).getPath
  private lazy val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  lazy val ramps: Vector[Ramp] = rampParser.loadRamps

  implicit lazy val streetGraph: StreetGraphContainer = LazyStreetGraphContainer()
  implicit lazy val sidewalkGraph: SidewalkGraphContainer = LazySidewalkGraphContainer()
}
