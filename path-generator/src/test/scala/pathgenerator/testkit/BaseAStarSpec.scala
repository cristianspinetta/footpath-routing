package pathgenerator.testkit

import mapdomain.graph._

trait BaseAStarSpec {

  protected val abstractGraphPrototype: InMemoryGraphContainer[GraphEdge, GraphVertex[GraphEdge]] = InMemoryGraphContainer(List(
    GraphVertex.createWithEdges(1, List(2, 6)),
    GraphVertex.createWithEdges(2, List(1, 3)),
    GraphVertex.createWithEdges(3, List(2, 7, 4)),
    GraphVertex.createWithEdges(4, List(3, 8)),
    GraphVertex.createWithEdges(5, List(6, 11)),
    GraphVertex.createWithEdges(6, List(1, 5)),
    GraphVertex.createWithEdges(7, List(3)),
    GraphVertex.createWithEdges(8, List(4, 9)),
    GraphVertex.createWithEdges(9, List(8, 14)),
    GraphVertex.createWithEdges(10, List(11, 12, 13)),
    GraphVertex.createWithEdges(11, List(5, 10)),
    GraphVertex.createWithEdges(12, List(10, 15)),
    GraphVertex.createWithEdges(13, List(10, 18)),
    GraphVertex.createWithEdges(14, List(9, 18)),
    GraphVertex.createWithEdges(15, List(12, 16)),
    GraphVertex.createWithEdges(16, List(15, 17)),
    GraphVertex.createWithEdges(17, List(16, 19)),
    GraphVertex.createWithEdges(18, List(13, 14, 19)),
    GraphVertex.createWithEdges(19, List(17, 18))))

  protected val geoGraphPrototype: InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = GraphContainer.createEagerGeoGraph(Map(
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
    14L -> (List(9L, 13L), Coordinate(4, 10))))

}
