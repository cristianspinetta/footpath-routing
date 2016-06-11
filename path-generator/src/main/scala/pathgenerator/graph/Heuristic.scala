package pathgenerator.graph

trait Heuristic[V <: Vertex] {
  def apply(node: V): Double
}

case class TrivialHeuristic[V <: Vertex]() extends Heuristic[V] {
  override def apply(node: V): Double = 0
}

case class GeoHeuristic[V <: GeoVertex](referenceNode: V) extends Heuristic[V] {
  override def apply(node: V): Double = referenceNode.coordinate.distanceTo(node.coordinate)
}
