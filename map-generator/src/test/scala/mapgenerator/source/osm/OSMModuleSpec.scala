package mapgenerator.source.osm

import org.scalatest.{FlatSpec, Matchers}
import pathgenerator.graph.GraphContainer

class OSMModuleSpec extends FlatSpec with BaseOSMSpec with Matchers {

  val xmlParser: OSMLoaderByXml = OSMLoaderByXml(osmURL)

  "With all OSM elements" should "create a graph correctly" in {

    val osmModule: OSMModule = OSMModule(xmlParser.loadNodes, xmlParser.loadWays)

    val graph: GraphContainer[OsmVertex] = osmModule.createGraph

    graph.vertices.size should be > 1
  }
}
