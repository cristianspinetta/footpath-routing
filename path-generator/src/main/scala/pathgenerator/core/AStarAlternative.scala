package pathgenerator.core

import pathgenerator.graph.{ Edge, GraphContainer, Node }

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class AStarAlternative[V <: Node](gMap: GraphContainer[V], startNode: Node, targetNode: Node,
    estFurtherCostFunc: Node ⇒ Int) {

  // priority queue orders by highest value, so cost is negated.
  val ordering = Ordering.by[MetaInformedState, Int] {
    case MetaInformedState(_, cost, estFurtherCost) ⇒ -(cost + estFurtherCost).toInt
    case _ ⇒ 0
  }

  // The set of tentative nodes to be evaluated, initially containing the start node
  private val _opens = mutable.PriorityQueue[MetaInformedState](MetaInformedState(startNode, 0, 0))(ordering)

  // The set of nodes already evaluated.
  private val _visited = mutable.Set[Node]()

  // The map of navigated nodes.
  private val _cameFrom: TrieMap[Node, Node] = TrieMap.empty

  def search: List[Edge] = loop(1, None)

  @tailrec
  private def loop(loopCount: Int, fromNode: Option[Node]): List[Edge] = {
    if (_opens isEmpty) Nil
    else {
      val current = _opens.dequeue()
      fromNode foreach (from ⇒ _cameFrom += ((current.node, from)))
      if (current.node == targetNode) {
        reconstructPath(_cameFrom, current.node)
      } else {
        enqueueNeighbours(current.node, current.cost)
        loop(loopCount + 1, Some(current.node))
      }
    }
  }

  private def enqueueNeighbours(node: Node, cost: Long) {
    val neighbours = node neighbours gMap
    val nonVisitedNeighbours = neighbours filter (v ⇒ !_visited.contains(v))
    _visited ++= nonVisitedNeighbours

    val metaNeighbours = nonVisitedNeighbours.map { v ⇒ MetaInformedState(v, cost + 1, estFurtherCostFunc(v)) }
    _opens.enqueue(metaNeighbours: _*)
  }

  // recursive function
  private def reconstructPath(cameFrom: TrieMap[Node, Node], current: Node): List[Edge] = {
    cameFrom.get(current) match {
      case Some(nodeFrom) ⇒
        reconstructPath(cameFrom - current, nodeFrom) ++ nodeFrom.getEdgesFor(current.id).toList
      case None ⇒ Nil
    }
  }

}

case class MetaInformedState(node: Node, cost: Long, estFurtherCost: Long)
