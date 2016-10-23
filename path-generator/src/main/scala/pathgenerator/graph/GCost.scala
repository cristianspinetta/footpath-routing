package pathgenerator.graph

import mapdomain.graph.{ Edge, Vertex }

trait GCost[E <: Edge, V <: Vertex[E]] {
  def calculate(from: V, to: V): Double
}

case class GeoGCost[E <: Edge, V <: Vertex[E]]() extends GCost[E, V] {
  def calculate(from: V, to: V): Double = from.distanceToNeighbour(to.id)
}
