package mapgenerator.source.osm

import org.scalatest.{ FlatSpec, Matchers }

class OSMLoaderByXmlSpec extends FlatSpec with BaseOSMSpec with Matchers {

  "A complete OSM file" should "be parsed correctly" in {
    val xmlParser: OSMLoaderByXml = OSMLoaderByXml(osmURL)

    val nodes: Seq[OSMNode] = xmlParser.loadNodes
    val ways: Seq[Way] = xmlParser.loadWays
    val relations: Seq[Relation] = xmlParser.loadRelations

    nodes.size should be(9335)
    ways.size should be(3194)
    relations.size should be(384)
  }

}
