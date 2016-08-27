package mapdomain.utils

import mapdomain.graph.{GeoEdge, GeoVertex, GraphContainer}
import mapdomain.math.{GVector, Line, Point}

object EdgeUtils {

  def edgeToLine[V <: GeoVertex, E <: GeoEdge](edge: E)(implicit graph: GraphContainer[V]): Line = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    Line.ByPairPoints(pointStart, pointEnd)
  }

  def edgeToVector[V <: GeoVertex, E <: GeoEdge](edge: E)(implicit graph: GraphContainer[V]): GVector = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    GVector(pointStart, pointEnd)
  }

  def pointToEdge[V <: GeoVertex](points: List[V], distance: Double = 10): Iterator[GeoEdge] = {
    points.sliding(2).flatMap {
      case fst :: snd :: Nil => Iterator(GeoEdge(fst.id, snd.id, distance))
      case _ => Iterator.empty
    }
  }
}
