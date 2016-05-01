package pathgenerator.core

import pathgenerator.graph.{ Edge, GraphContainer, Vertex }

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class AStarAlternative[V <: Vertex](gMap: GraphContainer[V], startVertex: Vertex, targetVertex: Vertex,
    estFurtherCostFunc: Vertex ⇒ Int) {

  // priority queue orders by highest value, so cost is negated.
  val ordering = Ordering.by[MetaInformedState, Int] {
    case MetaInformedState(_, cost, estFurtherCost) ⇒ -(cost + estFurtherCost).toInt
    case _ ⇒ 0
  }

  // The set of tentative nodes to be evaluated, initially containing the start node
  private val _opens = mutable.PriorityQueue[MetaInformedState](MetaInformedState(startVertex, 0, 0))(ordering)

  // The set of nodes already evaluated.
  private val _visited = mutable.Set[Vertex]()

  // The map of navigated nodes.
  private val _cameFrom: TrieMap[Vertex, Vertex] = TrieMap.empty

  def search: List[Edge] = loop(1, None)

  @tailrec
  private def loop(loopCount: Int, fromVertex: Option[Vertex]): List[Edge] = {
    if (_opens isEmpty) Nil
    else {
      val current = _opens.dequeue()
      fromVertex foreach (from ⇒ _cameFrom += ((current.vertex, from)))
      if (current.vertex == targetVertex) {
        reconstructPath(_cameFrom, current.vertex)
      } else {
        enqueueNeighbours(current.vertex, current.cost)
        loop(loopCount + 1, Some(current.vertex))
      }
    }
  }

  private def enqueueNeighbours(vertex: Vertex, cost: Long) {
    val neighbours = vertex neighbours gMap
    val nonVisitedNeighbours = neighbours filter (v ⇒ !_visited.contains(v))
    _visited ++= nonVisitedNeighbours

    val metaNeighbours = nonVisitedNeighbours.map { v ⇒ MetaInformedState(v, cost + 1, estFurtherCostFunc(v)) }
    _opens.enqueue(metaNeighbours: _*)
  }

  // recursive function
  private def reconstructPath(cameFrom: TrieMap[Vertex, Vertex], current: Vertex): List[Edge] = {
    cameFrom.get(current) match {
      case Some(vertexFrom) ⇒
        reconstructPath(cameFrom - current, vertexFrom) ++ vertexFrom.getEdgesFor(current.id).toList
      case None ⇒ Nil
    }
  }

}

case class MetaInformedState(vertex: Vertex, cost: Long, estFurtherCost: Long)
