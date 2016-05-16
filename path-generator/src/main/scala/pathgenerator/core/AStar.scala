package pathgenerator.core

import pathgenerator.graph._
import base.{ LazyLoggerSupport, MeterSupport }

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.{ Failure, Try }
import scala.reflect.runtime.universe._

/**
 * @see https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
 * @param heuristic: Heuristic Function
 * @param gMap: Graph accessing
 * @param startVertex: origin vertex
 * @param targetVertex: target vertex
 * @param tag: implicit TypeTag supplied by Scala Compiler
 * @tparam N: Vertex Type with which it works
 */
case class AStar[N <: Vertex](heuristic: Heuristic[N])(gMap: GraphContainer[N], startVertex: N, targetVertex: N)(implicit tag: TypeTag[N]) extends LazyLoggerSupport with MeterSupport {

  /**
   * The vertices already evaluated.
   */
  private val _closed = mutable.Set[N]()

  /**
   * The map of navigated vertices.
   * For each vertex, which vertex it can most efficiently be reached from.
   * If a vertex can be reached from many vertices, cameFrom will eventually contain the
   * most efficient previous step.
   */
  private val _cameFrom: mutable.Map[N, N] = TrieMap.empty

  /**
   * For each vertex, the cost of getting from the start vertex to that vertex.
   * The cost of going from start to start is zero.
   */
  private val gScore: mutable.Map[Long, Double] = TrieMap(startVertex.id -> 0D)
    .withDefaultValue(Double.MaxValue)

  /**
   * For each vertex, the total cost of getting from the start vertex to the goal
   * by passing by that vertex. That value is partly known, partly heuristic.
   * For the first vertex, that value is completely heuristic.
   */
  private val fScore: mutable.Map[Long, Double] = TrieMap(startVertex.id -> heuristic(startVertex))
    .withDefaultValue(Double.MaxValue)

  /**
   * Tentative vertices to be evaluated, initially containing the start vertex
   */
  private val _opensQueue: mutable.PriorityQueue[N] = mutable.PriorityQueue(startVertex)(Ordering.by[N, Double] { vertex: N ⇒
    -fScore(vertex.id)
  })

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

  @tailrec
  private def loop(nroLoop: Int): List[Edge] = {

    logCurrentState(nroLoop)

    if (_opensQueue isEmpty) Nil
    else {
      val current = _opensQueue.dequeue

      logger.debug(s"visit vertex ${current.id}")

      if (current == targetVertex)
        reconstructPath(_cameFrom, targetVertex)
      else {
        _closed.add(current)

        val ns: List[N] = current.neighbours(gMap)

        ns foreach eachNeighbour(current)

        if (_opensQueue nonEmpty) loop(nroLoop + 1)
        else throw new RuntimeException("failure!!")
      }
    }
  }

  private def eachNeighbour(current: N)(neighbour: N): Unit = {

    if (_closed.contains(neighbour)) {
      // Ignore the neighbor which is already evaluated.
    } else {

      val fromCurrentToNeighbour = current.distanceToNeighbour(neighbour.id)
      // The distance from start to a neighbor
      val tentativeGScore = gScore(current.id) + fromCurrentToNeighbour

      if (!_opensQueue.exists(_.id == neighbour.id)) { // Discover a new vertex
        // This path is the best until now. Record it!
        _cameFrom += (neighbour -> current)
        gScore += (neighbour.id -> tentativeGScore)
        fScore += (neighbour.id -> (tentativeGScore + heuristic(neighbour)))
        _opensQueue enqueue neighbour // must enqueue after the neighbour is added to fScore
      } else if (tentativeGScore >= gScore(neighbour.id)) {
        // This is not a better path.
      }
    }
  }

  private def reconstructPath(_cameFrom: mutable.Map[N, N], targetVertex: N)(implicit tag: TypeTag[N]): List[Edge] = {
    var current = targetVertex
    var totalPath = List(current)
    while (_cameFrom.contains(current)) {
      current = _cameFrom(current)
      totalPath = totalPath.:+(current)
    }
    totalPath.tail.foldLeft((List.empty[Edge], totalPath.head)) {
      case ((listEdges, beforeVertex), current: N) ⇒
        (current.getEdgesFor(beforeVertex.id).toList ::: listEdges, current)
    }._1
  }

  private def logCurrentState(nroLoop: Int): Unit = {
    logger.debug(s"-----------------------------------")
    logger.debug(s"Initialize loop #$nroLoop")
    logger.debug(s"-----------------------------------")
    logger.debug(s"opens: ${_opensQueue.map(_.id) mkString ", "}")
    logger.debug(s"closed: ${_closed.map(_.id) mkString ", "}")
    logger.debug(s"cameFrom: ${_cameFrom.map(tuple ⇒ (tuple._1.id, tuple._2.id)) mkString ", "}")
    logger.debug(s"G Score: ${gScore mkString " -> "}")
    logger.debug(s"Heuristic - F Score: ${fScore mkString " -> "}")
  }

}
