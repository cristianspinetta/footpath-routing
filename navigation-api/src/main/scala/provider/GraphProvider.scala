package provider

import base.LazyLoggerSupport
import base.conf.{ ApiEnvConfig, SidewalkGraphConf, StreetGraphConf }
import mapdomain.sidewalk._
import mapdomain.street._
import mapgenerator.source.features.{ RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV }

import scala.util.{ Failure, Success, Try }

trait GraphSupport extends ApiEnvConfig {
  import GraphSupport._

  def graphs: GraphSet = GraphSupport.getGraphSet
}

object GraphSupport extends GraphSupport with LazyLoggerSupport {

  @volatile private var _graphSet: Option[GraphSet] = None
  @volatile private var _status: GraphStatus = Uninitialized

  def getGraphSet: GraphSet = synchronized {
    val graph = _graphSet getOrElse {
      _status = Loading
      Try(GraphSet(
        street = StreetGraphFactory.createFromConfig(configuration.Graph.street),
        sidewalk = SidewalkGraphFactory.createFromConfig(configuration.Graph.sidewalk),
        streetDB = StreetGraphFactory.createDB,
        sidewalkDB = SidewalkGraphFactory.createDB)) match {
        case Success(graphSet) ⇒
          _status = Loaded
          _graphSet = Some(graphSet)
          graphSet
        case Failure(exc) ⇒
          val reason: String = s"Failed trying to load the graph set."
          logger.error(reason, exc)
          _status = Broken(reason)
          throw exc
      }
    }
    graph
  }

  def graphLoaded: Boolean = _status == Loaded
  def status: GraphStatus = _status

  case class GraphSet(street: StreetGraphContainer,
    sidewalk: SidewalkGraphContainer,
    streetDB: StreetGraphContainer,
    sidewalkDB: SidewalkGraphContainer)

  trait GraphStatus
  case object Uninitialized extends GraphStatus
  case object Loading extends GraphStatus
  case object Loaded extends GraphStatus
  case class Broken(reason: String) extends GraphStatus {
    override def toString: String = s"Broken: $reason."
  }
}

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
  override val streetGraph: StreetGraphContainer = InMemoryStreetGraphContainer.createFromSnapshot
}

private object SidewalkGraphProviderDB extends SidewalkGraphProvider with LazyLoggerSupport {
  override val sidewalkGraph: SidewalkGraphContainer = LazySidewalkGraphContainer()
}
private object SidewalkGraphProviderInMemory extends SidewalkGraphProvider with LazyLoggerSupport {
  override val sidewalkGraph: SidewalkGraphContainer = InMemorySidewalkGraphContainer.createFromSnapshot
}

