package mapgenerator.source.osm

import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.write
import org.scalatest.{ FlatSpec, Matchers }
import pathgenerator.graph.GraphContainer

import scala.collection.mutable.ListBuffer

class OSMModuleSpec extends FlatSpec with BaseOSMSpec with Matchers {

  implicit val formats = DefaultFormats

  val xmlParser: OSMLoaderByXml = OSMLoaderByXml(osmURL)
  val graphJsonParser: GraphJsonLoader = GraphJsonLoader(graphJsonURL)
  val intersectionVertexCount: Int = 1076

  "With all OSM elements" should "create a graph correctly" in {

    val osmModule: OSMModule = OSMModule(xmlParser.loadNodes, xmlParser.loadWays)

    val graph: GraphContainer[OsmVertex] = osmModule.createGraph

    graphJsonParser.vertices.size should be(intersectionVertexCount)

    graph.vertices.size should be >= 900

    val graphVertices: Seq[OsmVertex] = graph.vertices.toIndexedSeq
    val otpVertices: ListBuffer[OTPVertex] = ListBuffer(graphJsonParser.vertices: _*)

    for {
      vertex ← graphVertices
    } {
      val vertexIndex: Int = otpVertices.indexWhere(_.nodeId == vertex.id)
      withClue(s"Vertex not found: #${graphVertices.indexOf(vertex)} ${write(vertex)}.") {
        vertexIndex should be >= 0
      }
      val otpVertex: OTPVertex = otpVertices(vertexIndex)
      withClue(s"Different Vertex. Own Vertex: #${graphVertices.indexOf(vertex)} ${write(vertex)}. OTP Vertex: ${write(otpVertex)}") {
        vertex.coordinate.longitude shouldBe otpVertex.x
        vertex.coordinate.latitude shouldBe otpVertex.y
      }
      otpVertices.remove(vertexIndex)
    }
  }
}
