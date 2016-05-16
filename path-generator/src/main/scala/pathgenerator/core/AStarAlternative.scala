package pathgenerator.core

import pathgenerator.graph.{ Edge, GraphContainer, Vertex }

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class AStarAlternative[V <: Vertex](gMap: GraphContainer[V], startNode: Vertex, targetNode: Vertex,
                                    estFurtherCostFunc: Vertex ⇒ Int) {

  // priority queue orders by highest value, so cost is negated.
  val ordering = Ordering.by[MetaInformedState, Int] {
    case MetaInformedState(_, cost, estFurtherCost) ⇒ -(cost + estFurtherCost).toInt
    case _ ⇒ 0
  }

  // The set of tentative vertices to be evaluated, initially containing the start vertex
  private val _opens = mutable.PriorityQueue[MetaInformedState](MetaInformedState(startNode, 0, 0))(ordering)

  // The set of vertices already evaluated.
  private val _visited = mutable.Set[Vertex]()

  // The map of navigated vertices.
  private val _cameFrom: TrieMap[Vertex, Vertex] = TrieMap.empty

  def search: List[Edge] = loop(1, None)

  @tailrec
  private def loop(loopCount: Int, fromNode: Option[Vertex]): List[Edge] = {
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

  private def enqueueNeighbours(node: Vertex, cost: Long) {
    val neighbours = node neighbours gMap
    val nonVisitedNeighbours = neighbours filter (v ⇒ !_visited.contains(v))
    _visited ++= nonVisitedNeighbours

    val metaNeighbours = nonVisitedNeighbours.map { v ⇒ MetaInformedState(v, cost + 1, estFurtherCostFunc(v)) }
    _opens.enqueue(metaNeighbours: _*)
  }

  // recursive function
  private def reconstructPath(cameFrom: TrieMap[Vertex, Vertex], current: Vertex): List[Edge] = {
    cameFrom.get(current) match {
      case Some(nodeFrom) ⇒
        reconstructPath(cameFrom - current, nodeFrom) ++ nodeFrom.getEdgesFor(current.id).toList
      case None ⇒ Nil
    }
  }

}

case class MetaInformedState(node: Vertex, cost: Long, estFurtherCost: Long)
