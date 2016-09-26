package mapdomain.graph

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait GeoSearch[E <: GeoEdge, V <: GeoVertex[E]] { self: GraphContainer[E, V] ⇒

  protected val position: Coordinate

  def findNearest(vertex: V, distance: Double): List[V]
  //  def findNearest(vertex: V, distance: Double): List[V] = {
  //    self.vertices.filter(otherVertex => otherVertex.coordinate.distanceTo(vertex.coordinate) <= distance)
  //  }

}

object GeoSearch {

  def findNearestByRadius[T, C <: Traversable[T], That[T]](startPosition: Coordinate, radius: Double, otherElems: C,
    getPosition: T ⇒ Seq[Coordinate])(implicit bf: CanBuildFrom[C, T, That[T]]) = {
    otherElems.par.filter(other ⇒ getPosition(other).exists(_.distanceTo(startPosition) <= radius)).to[That]
  }
}
