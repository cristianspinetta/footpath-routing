package provider

import java.util.concurrent.atomic.AtomicReference

import base.conf.{ ApiEnvConfig, SidewalkGraphConf, StreetGraphConf }
import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.repository.sidewalk.RampRepository
import mapdomain.sidewalk._
import mapdomain.street.InMemoryStreetGraphContainer.{ apply ⇒ _, unapply ⇒ _, withTimeLogging ⇒ _ }
import mapdomain.street._
import snapshot.sidewalk.SidewalkVertexSnapshot
import snapshot.street.StreetVertexSnapshot

import scala.util.{ Failure, Success, Try }

trait GraphSupport extends ApiEnvConfig {
  import GraphSupport._

  def graphs: GraphSet = GraphSupport.getGraphSet
}

object GraphSupport extends GraphSupport with LazyLoggerSupport {

  private val _graphSet: AtomicReference[GraphSet] = new AtomicReference[GraphSet](GraphSet())
  private val _status: AtomicReference[GraphStatus] = new AtomicReference[GraphStatus](Uninitialized)

  def getGraphSet: GraphSet = synchronized {
    if (graphAvailable) _graphSet.get()
    else {
      _status.set(Loading)
      _graphSet.get().reload()
      _status.set(Loaded)
      _graphSet.get()
    }
  }

  def reload(): Unit = {
    logger.info(s"Reloading Graphs...")
    _status.set(Reloading)
    val newGraph = GraphSet().reload()
    _graphSet.set(newGraph)
    _status.set(Loaded)
    logger.info(s"Graphs reloaded successfully...")
  }

  def graphAvailable: Boolean = {
    val status = _status.get()
    status == Loaded || status == Reloading
  }

  def status: GraphStatus = _status.get()

  case class GraphSet() {

    private val _street = new AtomicReference[StreetGraphContainer]()
    private val _sidewalk = new AtomicReference[SidewalkGraphContainer]()
    private val _streetDB = new AtomicReference[StreetGraphContainer]()
    private val _sidewalkDB = new AtomicReference[SidewalkGraphContainer]()

    def reload(): GraphSet = {
      _street.set(StreetGraphFactory.createFromConfig(configuration.Graph.street))
      _streetDB.set(StreetGraphFactory.createDB)
      _sidewalk.set(SidewalkGraphFactory.createFromConfig(configuration.Graph.sidewalk))
      _sidewalkDB.set(SidewalkGraphFactory.createDB)
      this
    }

    def street: StreetGraphContainer = _street.get()
    def sidewalk: SidewalkGraphContainer = _sidewalk.get()
    def streetDB: StreetGraphContainer = _streetDB.get()
    def sidewalkDB: SidewalkGraphContainer = _sidewalkDB.get()
  }

  trait GraphStatus
  case object Uninitialized extends GraphStatus
  case object Loading extends GraphStatus
  case object Loaded extends GraphStatus
  case object Reloading extends GraphStatus
  case class Broken(reason: String) extends GraphStatus {
    override def toString: String = s"Broken: $reason."
  }
}

object StreetGraphFactory extends LazyLoggerSupport with MeterSupport {

  def createFromConfig(config: StreetGraphConf): StreetGraphContainer = if (config.inMemory) createInMemory else createDB

  def createDB: StreetGraphContainer = LazyStreetGraphContainer()

  def createInMemory: StreetGraphContainer = withTimeLogging({
    logger.info("Getting Street Graph from Snapshot")
    InMemoryStreetGraphContainer(StreetVertexSnapshot.get().toMap)
  }, (time: Long) ⇒ logger.info(s"Loading Street Graph from Snapshot finished in $time ms."))
}

object SidewalkGraphFactory extends LazyLoggerSupport with MeterSupport {

  def createFromConfig(config: SidewalkGraphConf): SidewalkGraphContainer = if (config.inMemory) createInMemory else createDB

  def createDB: SidewalkGraphContainer = LazySidewalkGraphContainer()

  def createInMemory: SidewalkGraphContainer = withTimeLogging({
    logger.info("Getting Sidewalk Graph from Snapshot")
    InMemorySidewalkGraphContainer(SidewalkVertexSnapshot.get().toMap)
  }, (time: Long) ⇒ logger.info(s"Loading Sidewalk Graph from Snapshot finished in $time ms."))
}
