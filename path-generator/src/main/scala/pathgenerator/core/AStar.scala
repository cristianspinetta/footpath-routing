package pathgenerator.core

import pathgenerator.graph._
import base.MeterSupport
import com.typesafe.scalalogging.LazyLogging

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.{ Failure, Try }
import scala.reflect.runtime.universe._

case class AStar[V <: Vertex](heuristic: Heuristic[V])(gMap: GraphContainer[V], startVertex: V, targetVertex: V)(implicit tag: TypeTag[V]) extends LazyLogging with MeterSupport {

  //  // priority queue orders by highest value, so cost is negated.
  //  val ordering = Ordering.by[MetaInformedState, Int] {
  //    case MetaInformedState(_, cost, estFurtherCost) ⇒ -(cost + estFurtherCost).toInt
  //    case _ ⇒ 0
  //  }
  //
  //  // The set of tentative nodes to be evaluated, initially containing the start node
  //  private val _opens = mutable.PriorityQueue[MetaInformedState](MetaInformedState(startVertex, 0, 0))(ordering)

  // The set of tentative nodes to be evaluated, initially containing the start node
  private var _opens: List[V] = List(startVertex) // FIXME change by Queue

  // The set of nodes already evaluated.
  private val _closed = mutable.Set[V]()

  // The map of navigated nodes.
  // For each node, which node it can most efficiently be reached from.
  // If a node can be reached from many nodes, cameFrom will eventually contain the
  // most efficient previous step.
  private val _cameFrom: mutable.Map[V, V] = TrieMap.empty

  // For each node, the cost of getting from the start node to that node.
  // The cost of going from start to start is zero.
  private val gScore: mutable.Map[Long, Double] = TrieMap(startVertex.id -> 0D)
    .withDefaultValue(Double.MaxValue)

  // For each node, the total cost of getting from the start node to the goal
  // by passing by that node. That value is partly known, partly heuristic.
  // For the first node, that value is completely heuristic.
  private val fScore: mutable.Map[Long, Double] = TrieMap(startVertex.id -> heuristic(startVertex))
    .withDefaultValue(Double.MaxValue)

  def search: Try[List[Edge]] = withTimeLoggingInMicro({
    Try {
      loop(1)
    } recoverWith {
      case ex: Throwable ⇒
        logger.error(s"Failure trying to find the short path at ${targetVertex.id}", ex)
        logCurrentState(0)
        Failure(ex)
    }
  }, { timing: Long ⇒
    logger.info(s"Found short path from ${startVertex.id} to ${targetVertex.id} in $timing µs.")
  })

  private def loop(nroLoop: Int): List[Edge] = {

    logCurrentState(nroLoop)

    if (_opens isEmpty) Nil
    else {
      _opens = _opens.sortWith { (v1, v2) ⇒ fScore(v1.id).compareTo(fScore(v2.id)) < 0 }
      val current = _opens.head

      logger.debug(s"visit vertex ${current.id}")

      if (current == targetVertex)
        reconstructPath(_cameFrom, targetVertex)
      else {
        _opens = _opens.tail
        _closed.add(current)

        val ns: List[V] = current.neighbours(gMap)

        ns foreach eachNeighbour(current)

        if (_opens.nonEmpty) loop(nroLoop + 1)
        else throw new RuntimeException("failure!!")
      }
    }
  }

  private def eachNeighbour(current: V)(neighbour: V): Unit = {

    if (_closed.contains(neighbour)) {
      // Ignore the neighbor which is already evaluated.
    } else {

      val fromCurrentToNeighbour = current.distanceToNeighbour(neighbour.id)
      // The distance from start to a neighbor
      val tentativeGScore = gScore(current.id) + fromCurrentToNeighbour

      if (!_opens.contains(neighbour)) { // Discover a new node
        _opens = _opens.+:(neighbour)
        // This path is the best until now. Record it!
        _cameFrom += (neighbour -> current)
        gScore += (neighbour.id -> tentativeGScore)
        fScore += (neighbour.id -> (tentativeGScore + heuristic(neighbour)))
      } else if (tentativeGScore >= gScore(neighbour.id)) {
        // This is not a better path.
      }
    }
  }

  private def reconstructPath(_cameFrom: mutable.Map[V, V], targetVertex: V)(implicit tag: TypeTag[V]): List[Edge] = {
    var current = targetVertex
    var totalPath = List(current)
    while (_cameFrom.contains(current)) {
      current = _cameFrom(current)
      totalPath = totalPath.:+(current)
    }
    totalPath.tail.foldLeft((List.empty[Edge], totalPath.head)) {
      case ((listEdges, beforeVertex), current: V) if typeOf[V] <:< typeOf[Vertex] ⇒
        (current.getEdgesFor(beforeVertex.id).toList ::: listEdges, current)
    }._1
  }

  private def logCurrentState(nroLoop: Int): Unit = {
    logger.debug(s"-----------------------------------")
    logger.debug(s"Initialize loop #$nroLoop")
    logger.debug(s"-----------------------------------")
    logger.debug(s"opens: ${_opens.map(_.id) mkString ", "}")
    logger.debug(s"closed: ${_closed.map(_.id) mkString ", "}")
    logger.debug(s"cameFrom: ${_cameFrom.map(tuple ⇒ (tuple._1.id, tuple._2.id)) mkString ", "}")
    logger.debug(s"G Score: ${gScore mkString " -> "}")
    logger.debug(s"Heuristic - F Score: ${fScore mkString " -> "}")
  }

}
