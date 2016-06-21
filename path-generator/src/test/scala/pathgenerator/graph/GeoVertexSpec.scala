package pathgenerator.graph

import org.scalatest.{ FlatSpec, Matchers }

class GeoVertexSpec extends FlatSpec with Matchers {

  implicit val geoGraphPrototype: GraphContainer[GeoVertex] = GraphContainer.createGeoNodes(Map(
    1L -> (List(2L, 6L), Coordinate(1, 1)),
    2L -> (List(1L, 3L, 4L), Coordinate(3, 1)),
    3L -> (List(2L), Coordinate(3, 4)),
    4L -> (List(2L, 7L), Coordinate(4, 1)),
    5L -> (List(6L, 11L), Coordinate(1, 14)),
    6L -> (List(1L, 5L), Coordinate(1, 8)),
    7L -> (List(4L, 8L), Coordinate(4, 4)),
    8L -> (List(7L, 9L), Coordinate(4, 6)),
    9L -> (List(8L, 14L), Coordinate(4, 8)),
    10L -> (List(11L, 13L), Coordinate(4, 18)),
    11L -> (List(5L, 10L, 12L), Coordinate(1, 18)),
    12L -> (List(11L), Coordinate(1, 22)),
    13L -> (List(14L, 10L), Coordinate(4, 13)),
    14L -> (List(9L, 13L), Coordinate(4, 10)),
    15L -> (List(16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L), Coordinate(0, 0)),
    16L -> (List(9L, 13L), Coordinate(4, 10)),
    17L -> (List(9L, 13L), Coordinate(-4, 10)),
    18L -> (List(9L, 13L), Coordinate(-4, -10)),
    19L -> (List(9L, 13L), Coordinate(4, -10)),
    20L -> (List(9L, 13L), Coordinate(-5, 0)),
    21L -> (List(9L, 13L), Coordinate(0, -5)),
    22L -> (List(9L, 13L), Coordinate(5, 0)),
    23L -> (List(9L, 13L), Coordinate(0, 5))))

  "GeoVertex" should "sort its edges by angle correctly" in {
    GeoVertex.sortEdgesByAngle(geoGraphPrototype.findVertex(5).get).map(_.vertexEnd) shouldBe List[Long](11, 6)
    GeoVertex.sortEdgesByAngle(geoGraphPrototype.findVertex(11).get).map(_.vertexEnd) shouldBe List[Long](12, 10, 5)
    GeoVertex.sortEdgesByAngle(geoGraphPrototype.findVertex(15).get).map(_.vertexEnd) shouldBe List[Long](23, 16, 22, 19, 21, 18, 20, 17)
  }
}
