package mapgenerator.street

import mapdomain.street.{ InMemoryStreetGraphContainer, StreetEdge, StreetVertex }
import mapgenerator.source.osm._
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization.write
import org.scalatest.{ FlatSpec, Matchers }

import scala.collection.mutable.ArrayBuffer

class StreetGraphModuleSpec extends FlatSpec with BaseOSMSpec with Matchers {

  implicit val formats = DefaultFormats

  val xmlReader: OSMReaderByXml = OSMReaderByXml(osmURL)
  val graphJsonParser: GraphJsonLoader = GraphJsonLoader(graphJsonURL)
  //  val intersectionVertexCount: Int = 1076

  val osmModule: OSMModule = OSMModule(xmlReader.loadNodes, xmlReader.loadWays, xmlReader.loadRelations)
  val streetGraphModule: StreetGraphModule = StreetGraphModule(osmModule)
  val graph: InMemoryStreetGraphContainer = streetGraphModule.createGraph

  val otpVertices: ArrayBuffer[OTPVertex] = ArrayBuffer(graphJsonParser.vertices: _*)

  /**
   * Missing Edge. Difference is OwnEdges - OTPEdges
   *
   * @param veretxId
   * @param ownEdges
   * @param otpEdges
   */
  case class MissingEdgesReport(veretxId: Long, ownEdges: List[StreetEdge], otpEdges: List[OTPEdge]) {
    val difference: Int = ownEdges.size - otpEdges.size
  }

  "With all OSM elements" should "create a graph correctly" in {

    //    graphJsonParser.vertices.size should be(intersectionVertexCount)

    graph.vertices.size should be(otpVertices.size)

    val graphVertices: Seq[StreetVertex] = graph.vertices.toIndexedSeq

    val edgesReport = StringBuilder.newBuilder

    var edgeFailed = 0
    val missingEdges: ArrayBuffer[MissingEdgesReport] = ArrayBuffer.empty

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

      if (vertex.edges.size != otpVertex.outgoingStreetEdges.size) missingEdges +=
        MissingEdgesReport(vertex.id, vertex.edges, otpVertex.outgoingStreetEdges)

      for {
        edge ← otpVertex.outgoingStreetEdges
      } {

        def sameEdge(e: StreetEdge): Boolean = e.vertexStartId == edge.startOsmNodeId && e.vertexEndId == edge.endOsmNodeId

        if (!vertex.edges.exists(sameEdge)) {

          val report = s"Edge #${edge.id} with start vertex ${edge.startOsmNodeId} and end vertex ${edge.endOsmNodeId}" +
            s" not found in vertex #${graphVertices.indexOf(vertex)} Vertex: ${write(vertex)}. OTP Edge: ${write(edge)}"

          edgesReport.append(report + "\n\n----\n\n")
          edgeFailed += 1
        }

        //        withClue(s"Edge #${edge.id} with start vertex ${edge.startOsmNodeId} and end vertex ${edge.endOsmNodeId}" +
        //          s" not found in vertex #${graphVertices.indexOf(vertex)} Vertex: ${write(vertex)}. OTP Edge: ${write(edge)}") {
        //          val sameEdge = (e: StreetEdge)  => (e.streetVertexStart.id == edge.startOsmNodeId && e.streetVertexEnd.id == edge.endOsmNodeId) || (e.streetVertexStart.id == edge.endOsmNodeId && e.streetVertexEnd.id == edge.startOsmNodeId)
        //             vertex.edges.exists(sameEdge) should be (true)
        //        }
      }

      //      println(edgesReport.toString)
      otpVertices.remove(vertexIndex)
    }
    if (edgeFailed > 0) {
      val graphSum = graph.vertices.map(_.edges.size).sum
      val otpSum = otpVertices.map(_.outgoingStreetEdges.size).sum

      logger.warn(s"Number of Edges in Graph: $graphSum. In OTP: $otpSum.")
      logger.warn(s"Failed edges: $edgeFailed")
      logger.warn(s"Some missing edges on ${missingEdges.size} vertices: \n${missingEdges.map(mE ⇒ mE.veretxId.toString + " : " + mE.difference) mkString ", "}\n")
    }
  }

}
