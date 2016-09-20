package module

import base.LazyLoggerSupport
import conf.{ ApiEnvConfig, SidewalkGraphConf, StreetGraphConf}
import mapdomain.sidewalk._
import mapdomain.street.{ EagerStreetGraphContainer, LazyStreetGraphContainer, StreetGraphContainer }
import mapgenerator.source.features.{ RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV }

trait GraphSupport extends ApiEnvConfig {

  val graphs: GraphSet = GraphSet(
    street = StreetGraphFactory.createFromConfig(configuration.Graph.street),
    sidewalk = SidewalkGraphFactory.createFromConfig(configuration.Graph.sidewalk),
    streetDB = StreetGraphFactory.createDB,
    sidewalkDB = SidewalkGraphFactory.createDB)

  case class GraphSet(street: StreetGraphContainer,
                              sidewalk: SidewalkGraphContainer,
                              streetDB: StreetGraphContainer,
                              sidewalkDB: SidewalkGraphContainer)
}
object GraphSupport extends GraphSupport

object StreetGraphFactory {
  def createFromConfig(config: StreetGraphConf): StreetGraphContainer = if (config.inMemory) createInMemory else createDB
  def createDB: StreetGraphContainer = StreetGraphProviderDB.streetGraph
  def createInMemory: StreetGraphContainer = StreetGraphProviderInMemory.streetGraph
}

object SidewalkGraphFactory {
  def createFromConfig(config: SidewalkGraphConf): SidewalkGraphContainer = if (config.inMemory) createInMemory else createDB
  def createDB: SidewalkGraphContainer = SidewalkGraphProviderDB.sidewalkGraph
  def createInMemory: SidewalkGraphContainer = SidewalkGraphProviderInMemory.sidewalkGraph
}

sealed trait StreetGraphProvider {
  def streetGraph: StreetGraphContainer
}
sealed trait SidewalkGraphProvider {
  def sidewalkGraph: SidewalkGraphContainer
}

private object StreetGraphProviderDB extends StreetGraphProvider with LazyLoggerSupport {
  override val streetGraph: StreetGraphContainer = LazyStreetGraphContainer()
}
private object StreetGraphProviderInMemory extends StreetGraphProvider with LazyLoggerSupport {
  override val streetGraph: StreetGraphContainer = EagerStreetGraphContainer.createFromDB
}

private object SidewalkGraphProviderDB extends SidewalkGraphProvider with LazyLoggerSupport {
  override val sidewalkGraph: SidewalkGraphContainer = LazySidewalkGraphContainer()
}
private object SidewalkGraphProviderInMemory extends SidewalkGraphProvider with LazyLoggerSupport {
  override val sidewalkGraph: SidewalkGraphContainer = EagerSidewalkGraphContainer.createFromDB
}

object RampProvider extends ApiEnvConfig {

  private lazy val rampPath2014: String = configuration.Ramp.sourceFile2014Path
  private lazy val rampPath2011: String = configuration.Ramp.sourceFile2011Path
  private lazy val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  lazy val ramps: Vector[Ramp] = rampParser.loadRamps
}
