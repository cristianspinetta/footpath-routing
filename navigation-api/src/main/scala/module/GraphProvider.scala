package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.sidewalk._
import mapdomain.street.{ EagerStreetGraphContainer, LazyStreetGraphContainer, StreetGraphContainer }
import mapgenerator.source.features.{ RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV }

trait GraphSupport extends ApiEnvConfig {
  val graphProvider: GraphProvider = if (configuration.Graph.inMemory) GraphProviderInMemory else GraphProviderDB
}
object GraphSupport extends GraphSupport

sealed trait GraphProvider {
  implicit val streetGraph: StreetGraphContainer
  implicit val sidewalkGraph: SidewalkGraphContainer
}

private object GraphProviderDB extends GraphProvider with LazyLoggerSupport {
  override implicit val streetGraph: StreetGraphContainer = LazyStreetGraphContainer()
  override implicit val sidewalkGraph: SidewalkGraphContainer = LazySidewalkGraphContainer()
}

private object GraphProviderInMemory extends GraphProvider with LazyLoggerSupport {
  override implicit val streetGraph: StreetGraphContainer = EagerStreetGraphContainer.createFromDB
  override implicit val sidewalkGraph: SidewalkGraphContainer = EagerSidewalkGraphContainer.createFromDB
}

object RampProvider extends ApiEnvConfig {

  private lazy val rampPath2014: String = configuration.Ramp.sourceFile2014Path
  private lazy val rampPath2011: String = configuration.Ramp.sourceFile2011Path
  private lazy val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  lazy val ramps: Vector[Ramp] = rampParser.loadRamps
}
