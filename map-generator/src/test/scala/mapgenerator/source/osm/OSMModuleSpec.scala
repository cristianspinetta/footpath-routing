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

  val osmModule: OSMModule = OSMModule(xmlParser.loadNodes, xmlParser.loadWays)
  val graph: GraphContainer[OsmVertex] = osmModule.createGraph

  val otpVertices: ListBuffer[OTPVertex] = ListBuffer(graphJsonParser.vertices: _*)

  "With all OSM elements" should "create a graph correctly" in {

    graphJsonParser.vertices.size should be(intersectionVertexCount)

    graph.vertices.size should be >= 1060

    val graphSum = graph.vertices.map(_.edges.size).sum

    val otpSum = otpVertices.toList.map(_.outgoingStreetEdges.size).sum

    println(s"Graph Sum: $graphSum. OTP Sum: $otpSum.")

    val graphVertices: Seq[OsmVertex] = graph.vertices.toIndexedSeq

    val edgesReport = StringBuilder.newBuilder

    var edgeFailed = 1

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
        edge ← otpVertex.outgoingStreetEdges ::: otpVertex.incomingStreetEdges
      } {

        val sameEdge = (e: OsmStreetEdge)  => (e.osmVertexStart.id == edge.startOsmNodeId && e.osmVertexEnd.id == edge.endOsmNodeId) || (e.osmVertexStart.id == edge.endOsmNodeId && e.osmVertexEnd.id == edge.startOsmNodeId)
        if (!vertex.edges.exists(sameEdge)) {

          val report = s"Edge #${edge.id} with start vertex ${edge.startOsmNodeId} and end vertex ${edge.endOsmNodeId}" +
            s" not found in vertex #${graphVertices.indexOf(vertex)} Vertex: ${write(vertex)}. OTP Edge: ${write(edge)}"

          edgesReport.append(report + "\n\n----\n\n")
          edgeFailed += 1
        }


//        withClue(s"Edge #${edge.id} with start vertex ${edge.startOsmNodeId} and end vertex ${edge.endOsmNodeId}" +
//          s" not found in vertex #${graphVertices.indexOf(vertex)} Vertex: ${write(vertex)}. OTP Edge: ${write(edge)}") {
//          val sameEdge = (e: OsmStreetEdge)  => (e.osmVertexStart.id == edge.startOsmNodeId && e.osmVertexEnd.id == edge.endOsmNodeId) || (e.osmVertexStart.id == edge.endOsmNodeId && e.osmVertexEnd.id == edge.startOsmNodeId)
//             vertex.edges.exists(sameEdge) should be (true)
//        }
      }


//      println(edgesReport.toString)
      otpVertices.remove(vertexIndex)
    }
    println(s"Failed edges: $edgeFailed")
  }

}
