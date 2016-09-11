package module

import java.net.URL

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.GraphContainer
import mapdomain.sidewalk._
import mapdomain.street.{ EagerStreetGraphContainer, StreetEdge, StreetVertex }
import mapgenerator.sidewalk.SidewalkModule
import mapgenerator.source.features.{ RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV }
import mapgenerator.source.osm._
import mapgenerator.street.StreetGraphModule

trait GraphSupport {
  val graphProvider = GraphProvider
}

object GraphProvider extends LazyLoggerSupport with ApiEnvConfig {

  private lazy val osmURL: URL = getClass.getResource(configuration.OSM.sourceFilePath)
  private lazy val rampPath2014: String = getClass.getResource(configuration.Ramp.sourceFile2014Path).getPath
  private lazy val rampPath2011: String = getClass.getResource(configuration.Ramp.sourceFile2011Path).getPath

  private lazy val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)
  private lazy val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  lazy val osmModule: OSMModule = OSMModule(xmlParser)

  private lazy val streetGraphModule: StreetGraphModule = StreetGraphModule(osmModule)

  lazy val streetGraph: EagerStreetGraphContainer = streetGraphModule.createGraph.purgeStreets

  private lazy val sidewalkModule = SidewalkModule()(streetGraph)

  lazy val ramps: Vector[Ramp] = rampParser.loadRamps

  lazy val streets: List[StreetEdge] = for {
    vertex ← streetGraph.vertices
    edge ← vertex.edges
  } yield edge

  lazy val sidewalkGraph: EagerSidewalkGraphContainer = sidewalkModule.createSideWalks(failureTolerance = true).purgeSidewalks

  lazy val sidewalks = sidewalkGraph.sidewalkEdges
  lazy val streetCrossingEdges = sidewalkGraph.streetCrossingEdges

}
