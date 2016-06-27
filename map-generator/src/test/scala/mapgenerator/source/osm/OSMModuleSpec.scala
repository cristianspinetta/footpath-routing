package mapgenerator.source.osm

import mapgenerator.source.osm.graph.{OsmStreetEdge, OsmVertex}
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.write
import org.scalatest.{FlatSpec, Matchers}
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

    graph.vertices.size should be >= 1060

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

      for {
        edge ← otpVertex.outgoing ::: otpVertex.incoming
      } {
        withClue(s"Edge #${edge.id} with start vertex ${edge.startOsmNodeId} and end vertex ${edge.endOsmNodeId} not found in vertex #${graphVertices.indexOf(vertex)} ${write(vertex)}") {
          val sameEdge = (e: OsmStreetEdge)  => (e.osmVertexStart.id == edge.startOsmNodeId && e.osmVertexEnd.id == edge.endOsmNodeId) || (e.osmVertexStart.id == edge.endOsmNodeId && e.osmVertexEnd.id == edge.startOsmNodeId)
          vertex.edges.exists(sameEdge) should be (true)
        }
      }

      otpVertices.remove(vertexIndex)
    }
  }

}
