package pathgenerator.graph

trait Heuristic[V <: Vertex] {
  def apply(node: V): Double
}

case class TrivialHeuristic[V <: Vertex]() extends Heuristic[V] {
  override def apply(node: V): Double = 0
}

case class GeoHeuristic(referenceNode: GeoVertex) extends Heuristic[GeoVertex] {
  override def apply(node: GeoVertex): Double = referenceNode.coordinate.distanceTo(node.coordinate)
}
