package mapgenerator.sidewalk

import org.scalatest.{ FlatSpec, Matchers }
import pathgenerator.graph.{ Coordinate, GeoVertex, GraphContainer }

class SidewalkModuleSpec extends FlatSpec with Matchers {

  implicit val geoGraphPrototype: GraphContainer[GeoVertex] = GraphContainer.createGeoNodes(Map(
    1L -> (List(2L, 4L), Coordinate(0, 0)),
    2L -> (List(1L, 3L, 5L), Coordinate(0, 10)),
    3L -> (List(2L, 5L, 6L), Coordinate(0, 20)),
    4L -> (List(1L, 5L, 7L), Coordinate(-10, 0)),
    5L -> (List(2L, 3L, 4L, 6L, 8L, 9L), Coordinate(-10, 10)),
    6L -> (List(3L, 5L, 9L), Coordinate(-10, 20)),
    7L -> (List(4L, 8L), Coordinate(-20, 0)),
    8L -> (List(5L, 7L, 9L), Coordinate(-20, 10)),
    9L -> (List(5L, 6L, 8L), Coordinate(-20, 20))))

  "The SidewalkModule" should "create all sidewalk for a GeoVertex Graph" in {

    val sidewalkModule = SidewalkModule()

    val sideWalks: Set[SidewalkEdge] = sidewalkModule.createSideWalks

    sideWalks.size should be(28)

  }
}
