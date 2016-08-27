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

  def pointToEdge[V, E](points: List[V], edgeGenerator: (V, V) => E): List[E] = {
    points.sliding(2).toList.flatMap {
      case fst :: snd :: Nil => List(edgeGenerator(fst, snd))
      case _ => Nil
    }
  }
}
