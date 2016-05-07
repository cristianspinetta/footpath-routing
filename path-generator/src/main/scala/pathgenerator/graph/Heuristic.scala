package pathgenerator.graph

trait Heuristic[V <: Node] {
  def apply(node: V): Double
}

case class TrivialHeuristic[V <: Node]() extends Heuristic[V] {
  override def apply(node: V): Double = 0
}

case class GeoHeuristic(referenceNode: GeoNode) extends Heuristic[GeoNode] {
  override def apply(node: GeoNode): Double = referenceNode.coordinate.distanceTo(node.coordinate)
}
