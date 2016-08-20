package mapdomain.utils

import mapdomain.graph.{ GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.{ GVector, Line, Point }

object EdgeUtils {

  def edgeToLine(edge: GeoEdge)(implicit graph: GraphContainer[GeoVertex]): Line = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    Line.ByPairPoints(pointStart, pointEnd)
  }

  def edgeToVector(edge: GeoEdge)(implicit graph: GraphContainer[GeoVertex]): GVector = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    GVector(pointStart, pointEnd)
  }
}
