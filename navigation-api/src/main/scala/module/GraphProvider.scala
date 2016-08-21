package module

import java.net.URL

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.GraphContainer
import mapdomain.sidewalk.Ramp
import mapdomain.street.{ OsmStreetEdge, OsmVertex }
import mapgenerator.source.osm._

trait GraphSupport {
  val graphProvider = GraphProvider
}

object GraphProvider extends LazyLoggerSupport with ApiEnvConfig {

  lazy val osmURL: URL = getClass.getResource(configuration.OSM.sourceFilePath)
  lazy val rampPath2014: String = getClass.getResource(configuration.Ramp.sourceFile2014Path).getPath
  lazy val rampPath2011: String = getClass.getResource(configuration.Ramp.sourceFile2011Path).getPath

  lazy val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)
  lazy val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  lazy val graphModule: GraphModule = GraphModule(OSMModule(xmlParser))
  lazy val graph: GraphContainer[OsmVertex] = graphModule.createGraph

  lazy val ramps: Vector[Ramp] = rampParser.loadRamps

  lazy val streets: List[OsmStreetEdge] = for {
    vertex ← graph.vertices
    edge ← vertex.edges
  } yield edge

}
