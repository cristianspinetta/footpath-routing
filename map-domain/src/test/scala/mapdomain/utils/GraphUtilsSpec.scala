package mapdomain.utils

import mapdomain.graph._
import org.scalatest.{ FlatSpec, Matchers }

import scala.math._

class GraphUtilsSpec extends FlatSpec with Matchers {

  protected val geoGraph20: InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = GraphUtils.createGridGeoGraph(20, 20, 0)
  protected val geoGraph10: InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = GraphUtils.createGridGeoGraph(10, 10, 20 * 20 + 1)
  protected val geoGraph15: InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = GraphUtils.createGridGeoGraph(15, 15, 20 * 20 + 1 + 10 * 10 + 1)
  protected val geoGraph3: InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = GraphUtils.createGridGeoGraph(3, 3, 20 * 20 + 1 + 10 * 10 + 1 + 15 * 15 + 1)

  "GraphUtils" should "split a graph with some connected subgraphs" in {
    val graph: InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = InMemoryGraphContainer.joinGraphs[GeoEdge, GeoVertex[GeoEdge], InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]]](List(geoGraph20, geoGraph10, geoGraph15, geoGraph3), InMemoryGeoGraphContainer.apply[GeoEdge, GeoVertex[GeoEdge]])
    val connectedGraphs: List[List[GeoVertex[GeoEdge]]] = GraphUtils.splitByConnectedGraph[GeoEdge, GeoVertex[GeoEdge], InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]]](graph, InMemoryGeoGraphContainer.apply)

    connectedGraphs.size should be(4)
    connectedGraphs.map(_.size) should contain only (pow(20, 2).toInt, pow(10, 2).toInt, pow(15, 2).toInt, pow(3, 2).toInt)
  }

  it should "obtain the maximal connected graph" in {
    val graph: InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = InMemoryGraphContainer.joinGraphs[GeoEdge, GeoVertex[GeoEdge], InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]]](List(geoGraph20, geoGraph10, geoGraph15, geoGraph3), InMemoryGeoGraphContainer.apply)
    val connectedGraph = GraphUtils.getConnectedComponent[GeoEdge, GeoVertex[GeoEdge], InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]]](graph, InMemoryGeoGraphContainer.apply)
    val vertexIds = connectedGraph.vertices.map(_.id).distinct
    vertexIds.size should be(20 * 20)
    geoGraph20.vertices foreach { vertex ⇒
      vertexIds should contain(vertex.id)
    }
  }

}
