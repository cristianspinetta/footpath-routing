package mapdomain.utils

import mapdomain.graph.{ GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.{ GVector, Line, Point }

import scala.math.Ordering

object EdgeUtils {

  def edgeToLine[E <: GeoEdge, V <: GeoVertex[E]](edge: E)(implicit graph: GraphContainer[E, V]): Line = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    Line.ByPairPoints(pointStart, pointEnd)
  }

  def edgeToVector[E <: GeoEdge, V <: GeoVertex[E]](edge: E)(implicit graph: GraphContainer[E, V]): GVector = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    GVector(pointStart, pointEnd)
  }

  def pointToEdge[V, E](points: List[V], edgeGenerator: (V, V) ⇒ E): List[E] = {
    points.sliding(2).toList.flatMap {
      case fst :: snd :: Nil ⇒ List(edgeGenerator(fst, snd))
      case _                 ⇒ Nil
    }
  }

  // FIXME improve this
  def sortEdgesByAngle[E <: GeoEdge, V <: GeoVertex[E], G <: GraphContainer[E, V]](from: V, edges: List[E])(implicit graph: G): List[E] = {
    edges.sortBy(edge ⇒ { // FIXME: Ver de mejorar el ordenamiento
      from.coordinate.angleTo(graph.findVertex(edge.vertexEndId).get.coordinate)
    })(Ordering[Double])
  }
}
