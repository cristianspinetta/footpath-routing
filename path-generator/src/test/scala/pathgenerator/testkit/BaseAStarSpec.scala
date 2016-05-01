package pathgenerator.testkit

import pathgenerator.graph.{ Coordinate, GeoVertex, GraphContainer, GraphVertex }

trait BaseAStarSpec {

  protected val abstractGraphPrototype: GraphContainer[GraphVertex] = GraphContainer(List(
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

  protected val geoGraphPrototype: GraphContainer[GeoVertex] = GraphContainer(List(
    GeoVertex.createWithEdges(1, List(2, 6), Coordinate(0, 0)),
    GeoVertex.createWithEdges(2, List(1, 3), Coordinate(0, 0)),
    GeoVertex.createWithEdges(3, List(2, 7, 4), Coordinate(0, 0)),
    GeoVertex.createWithEdges(4, List(3, 8), Coordinate(0, 0)),
    GeoVertex.createWithEdges(5, List(6, 11), Coordinate(0, 0)),
    GeoVertex.createWithEdges(6, List(1, 5), Coordinate(0, 0)),
    GeoVertex.createWithEdges(7, List(3), Coordinate(0, 0)),
    GeoVertex.createWithEdges(8, List(4, 9), Coordinate(0, 0)),
    GeoVertex.createWithEdges(9, List(8, 14), Coordinate(0, 0)),
    GeoVertex.createWithEdges(10, List(11, 12, 13), Coordinate(0, 0)),
    GeoVertex.createWithEdges(11, List(5, 10), Coordinate(0, 0)),
    GeoVertex.createWithEdges(12, List(10, 15), Coordinate(0, 0)),
    GeoVertex.createWithEdges(13, List(10, 18), Coordinate(0, 0)),
    GeoVertex.createWithEdges(14, List(9, 18), Coordinate(0, 0)),
    GeoVertex.createWithEdges(15, List(12, 16), Coordinate(0, 0)),
    GeoVertex.createWithEdges(16, List(15, 17), Coordinate(0, 0)),
    GeoVertex.createWithEdges(17, List(16, 19), Coordinate(0, 0)),
    GeoVertex.createWithEdges(18, List(13, 14, 19), Coordinate(0, 0)),
    GeoVertex.createWithEdges(19, List(17, 18), Coordinate(0, 0))))

}
