package mapgenerator.source.osm

import mapgenerator.source.osm.model._
import org.scalatest.{ FlatSpec, Matchers }

class OSMLoaderByXmlSpec extends FlatSpec with BaseOSMSpec with Matchers {

  "A complete OSM file" should "be parsed correctly" in {
    val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)

    val nodes: Seq[OSMNode] = xmlParser.loadNodes
    val ways: Seq[Way] = xmlParser.loadWays
    val relations: Seq[Relation] = xmlParser.loadRelations

    nodes.size should be(34718)
    ways.size should be(13495)
    relations.size should be(938)
  }

}
