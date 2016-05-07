package pathgenerator.testkit

import pathgenerator.graph.{ Coordinate, GeoNode, GraphContainer, GraphNode }

trait BaseAStarSpec {

  protected val abstractGraphPrototype: GraphContainer[GraphNode] = GraphContainer(List(
    GraphNode.createWithEdges(1, List(2, 6)),
    GraphNode.createWithEdges(2, List(1, 3)),
    GraphNode.createWithEdges(3, List(2, 7, 4)),
    GraphNode.createWithEdges(4, List(3, 8)),
    GraphNode.createWithEdges(5, List(6, 11)),
    GraphNode.createWithEdges(6, List(1, 5)),
    GraphNode.createWithEdges(7, List(3)),
    GraphNode.createWithEdges(8, List(4, 9)),
    GraphNode.createWithEdges(9, List(8, 14)),
    GraphNode.createWithEdges(10, List(11, 12, 13)),
    GraphNode.createWithEdges(11, List(5, 10)),
    GraphNode.createWithEdges(12, List(10, 15)),
    GraphNode.createWithEdges(13, List(10, 18)),
    GraphNode.createWithEdges(14, List(9, 18)),
    GraphNode.createWithEdges(15, List(12, 16)),
    GraphNode.createWithEdges(16, List(15, 17)),
    GraphNode.createWithEdges(17, List(16, 19)),
    GraphNode.createWithEdges(18, List(13, 14, 19)),
    GraphNode.createWithEdges(19, List(17, 18))))

  protected val geoGraphPrototype: GraphContainer[GeoNode] = GraphContainer.createGeoNodes(Map(
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
