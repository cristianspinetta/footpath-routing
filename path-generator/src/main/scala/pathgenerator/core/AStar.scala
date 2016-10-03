package pathgenerator.core

import pathgenerator.graph._
import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph.{ Edge, GraphContainer, Vertex }

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
 * @param targetVertices: variable argument of the target vertices
 * @param tag: implicit TypeTag supplied by Scala Compiler
 * @tparam N: Vertex Type with which it works
 */
case class AStar[E <: Edge, N <: Vertex[E], M <: Heuristic[E, N]](heuristic: M)(gMap: GraphContainer[E, N], startVertex: N, targetVertices: N*)(implicit tag: TypeTag[N]) extends LazyLoggerSupport with MeterSupport {

  private val targetIds: List[Long] = targetVertices.map(_.id).toList

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

  def search: Try[List[E]] = withTimeLogging({
    Try {
      loop(1)
    } recoverWith {
      case ex: Throwable ⇒
        logger.error(s"Failure trying to find the short path at ${this.printTargetVertex}", ex)
        logCurrentState(0)
        Failure(ex)
    }
  }, { timing: Long ⇒
    logger.info(s"Found short path from ${startVertex.id} to ${this.printTargetVertex} in $timing ms.")
  })

  @tailrec
  private def loop(nroLoop: Int): List[E] = {

    logCurrentState(nroLoop)

    if (_opensQueue isEmpty) Nil
    else {
      val current = _opensQueue.dequeue

      logger.debug(s"visit vertex ${current.id}")

      if (reachTarget(current)) {
        logger.info(s"Reached the target vertex in $nroLoop loops.")
        reconstructPath(_cameFrom, current)
      } else {
        _closed.add(current)

        val ns: List[N] = gMap neighbours current

        ns foreach eachNeighbour(current)

        if (_opensQueue nonEmpty) loop(nroLoop + 1)
        else throw new RuntimeException(s"Failed searching the short path due to the open queue in A* is empty. Loop $nroLoop")
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

  private def reconstructPath(_cameFrom: mutable.Map[N, N], targetVertex: N)(implicit tag: TypeTag[N]): List[E] = {
    var current = targetVertex
    var totalPathBuilder = List.newBuilder[N]
    totalPathBuilder += current
    while (_cameFrom.contains(current)) {
      current = _cameFrom(current)
      totalPathBuilder += current
    }
    val totalPath: List[N] = totalPathBuilder.result()
    val (edges: List[E], _) = totalPath.tail.foldLeft((List.empty[E], totalPath.head)) {
      case ((listEs, beforeVertex), current: N) ⇒
        current.getEdgesFor(beforeVertex.id) match {
          case Some(edge) ⇒ (edge :: listEs, current)
          case None ⇒
            logger.warn(s"Could not find the edge between the vectors ${beforeVertex.id} and ${current.id}")
            (listEs, current)
        }
    }
    edges
  }

  private def logCurrentState(nroLoop: Int): Unit = {
    if (nroLoop % 100 == 0) logger.info(s"Loop $nroLoop looking for the target vertex.")
    logger.debug(s"-----------------------------------")
    logger.debug(s"Initialize loop #$nroLoop")
    logger.debug(s"-----------------------------------")
    logger.debug(s"opens: ${_opensQueue.map(_.id) mkString ", "}")
    logger.debug(s"closed: ${_closed.map(_.id) mkString ", "}")
    logger.debug(s"cameFrom: ${_cameFrom.map(tuple ⇒ (tuple._1.id, tuple._2.id)) mkString ", "}")
    logger.debug(s"G Score: ${gScore mkString " -> "}")
    logger.debug(s"Heuristic - F Score: ${fScore mkString " -> "}")
  }

  private def printTargetVertex: String = {
    targetVertices.map(_.id) mkString ", "
  }

  private def reachTarget(vertex: N): Boolean = targetIds.contains(vertex.id)

}
