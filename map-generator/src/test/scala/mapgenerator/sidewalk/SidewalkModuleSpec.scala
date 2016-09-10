package mapgenerator.sidewalk

import mapdomain.graph.{ Coordinate, GeoVertex, GraphContainer }
import mapdomain.sidewalk.{ SidewalkEdge, EdgeSidewalkGraphContainer, SidewalkVertex, StreetCrossingEdge }
import mapdomain.utils.GraphUtils
import org.scalatest.{ FlatSpec, Matchers }

class SidewalkModuleSpec extends FlatSpec with Matchers {

  implicit val geoGraphPrototype: GraphContainer[GeoVertex] = GraphContainer.createGeoNodes(Map(
    1L -> (List(2L, 4L), Coordinate(0, 0)),
    2L -> (List(1L, 3L, 5L), Coordinate(0, 10)),
    3L -> (List(2L, 5L, 6L), Coordinate(0, 20)),
    4L -> (List(1L, 5L, 7L), Coordinate(-10, 0)),
    5L -> (List(2L, 3L, 4L, 6L, 8L, 9L), Coordinate(-10, 10)),
    6L -> (List(3L, 5L, 9L, 10L), Coordinate(-10, 20)),
    7L -> (List(4L, 8L), Coordinate(-20, 0)),
    8L -> (List(5L, 7L, 9L), Coordinate(-20, 10)),
    9L -> (List(5L, 6L, 8L), Coordinate(-20, 20)),
    10L -> (List(6L), Coordinate(-10, 30))))

  val unconnectedGeoGraphPrototype: GraphContainer[GeoVertex] = GraphContainer.createGeoNodes(Map(
    1L -> (List(2L, 4L), Coordinate(0, 0)),
    2L -> (List(1L, 3L, 5L), Coordinate(0, 10)),
    3L -> (List(2L, 5L, 6L), Coordinate(0, 20)),
    4L -> (List(1L, 5L, 7L), Coordinate(-10, 0)),
    5L -> (List(2L, 3L, 4L, 6L, 8L, 9L), Coordinate(-10, 10)),
    6L -> (List(3L, 5L, 9L, 10L), Coordinate(-10, 20)),
    7L -> (List(4L, 8L), Coordinate(-20, 0)),
    8L -> (List(5L, 7L, 9L), Coordinate(-20, 10)),
    9L -> (List(5L, 6L, 8L), Coordinate(-20, 20)),
    10L -> (List(6L), Coordinate(-10, 30)),
    11L -> (List(12L), Coordinate(-50, 40)),
    12L -> (List(11L, 13L), Coordinate(-50, 50)),
    13L -> (List(12L), Coordinate(-50, 60))))

  "The SidewalkModule" should "create all sidewalk for a GeoVertex Graph" in {
    val sidewalkModule = SidewalkModule()
    val sidewalkGraphContainer: EdgeSidewalkGraphContainer = sidewalkModule.createSideWalks(distanceToStreet = 1)
    sidewalkGraphContainer.vertices.size should be(31)
    sidewalkGraphContainer.sidewalkEdges.size should be(30)
    sidewalkGraphContainer.streetCrossingEdges.size should be(28)

  }

  it should "get a single maximum graph that is connected" in {
    val sidewalkModule = SidewalkModule()(unconnectedGeoGraphPrototype)
    val sidewalkGraphContainer: EdgeSidewalkGraphContainer = sidewalkModule.createSideWalks(distanceToStreet = 1)
    val connectedGraph = sidewalkGraphContainer.purge
    connectedGraph.sidewalkEdges.size should be(30)
    connectedGraph.streetCrossingEdges.size should be(28)
    GraphUtils.isGraphConnected(connectedGraph) should be(true)

  }
}
