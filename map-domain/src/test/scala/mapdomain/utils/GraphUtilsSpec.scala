package mapdomain.utils

import mapdomain.graph.{ EagerGeoGraphContainer, EagerGraphContainer, GeoVertex, GraphContainer }
import org.scalatest.{ FlatSpec, Matchers }

import scala.math._

class GraphUtilsSpec extends FlatSpec with Matchers {

  protected val geoGraph20: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(20, 20, 0)
  protected val geoGraph10: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(10, 10, 20 * 20 + 1)
  protected val geoGraph15: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(15, 15, 20 * 20 + 1 + 10 * 10 + 1)
  protected val geoGraph3: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(3, 3, 20 * 20 + 1 + 10 * 10 + 1 + 15 * 15 + 1)

  "GraphUtils" should "split a graph with some connected subgraphs" in {
    val graph: EagerGeoGraphContainer[GeoVertex] = EagerGraphContainer.joinGraphs(List(geoGraph20, geoGraph10, geoGraph15, geoGraph3), EagerGeoGraphContainer.apply)
    val connectedGraphs: List[List[GeoVertex]] = GraphUtils.splitByConnectedGraph[GeoVertex, EagerGeoGraphContainer[GeoVertex]](graph, EagerGeoGraphContainer.apply)

    connectedGraphs.size should be(4)
    connectedGraphs.map(_.size) should contain only (pow(20, 2).toInt, pow(10, 2).toInt, pow(15, 2).toInt, pow(3, 2).toInt)
  }

  it should "obtain the maximal connected graph" in {
    val graph: EagerGeoGraphContainer[GeoVertex] = EagerGraphContainer.joinGraphs(List(geoGraph20, geoGraph10, geoGraph15, geoGraph3), EagerGeoGraphContainer.apply)
    val connectedGraph = GraphUtils.getConnectedComponent[GeoVertex, EagerGeoGraphContainer[GeoVertex]](graph, EagerGeoGraphContainer.apply)
    connectedGraph.vertices.size should be(20 * 20)
  }

}
