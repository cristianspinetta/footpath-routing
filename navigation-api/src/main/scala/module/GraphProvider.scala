package module

import java.net.URL

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.GraphContainer
import mapdomain.sidewalk.{Ramp, SidewalkEdge, StreetCrossingEdge}
import mapdomain.street.{OsmStreetEdge, OsmVertex}
import mapgenerator.sidewalk.SidewalkModule
import mapgenerator.source.osm._

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

  private lazy val graphModule: GraphModule = GraphModule(osmModule)

  implicit lazy val graph: GraphContainer[OsmVertex] = graphModule.createGraph

  private lazy val sidewalkModule = SidewalkModule()

  lazy val ramps: Vector[Ramp] = rampParser.loadRamps

  lazy val streets: List[OsmStreetEdge] = for {
    vertex ← graph.vertices
    edge ← vertex.edges
  } yield edge

  lazy val (sidewalks: Set[SidewalkEdge], streetCrossingEdges: Set[StreetCrossingEdge]) = sidewalkModule.createSideWalks()

}
