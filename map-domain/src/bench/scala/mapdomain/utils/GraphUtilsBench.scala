package mapdomain.utils

import mapdomain.graph._
import org.scalameter.Bench
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import scala.annotation.tailrec

object GraphUtilsBench extends Bench[Double] {

  /* configuration */

  lazy val executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default,
    Aggregator.max[Double],
    measurer
  )
//  lazy val measurer = new Measurer.Default
  override def measurer = new Executor.Measurer.MemoryFootprint
  lazy val reporter = new LoggingReporter[Double]
  lazy val persistor = Persistor.None

  /* inputs */

  protected val geoGraph20: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(20, 20, 0)
  protected val geoGraph10: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(10, 10, 20 * 20 + 1)
  protected val geoGraph15: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(15, 15, 20 * 20 + 1 + 10 * 10 + 1)
  protected val geoGraph3: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(3, 3, 20 * 20 + 1 + 10 * 10 + 1 + 15 * 15 + 1)
  protected val geoGraph50: EagerGeoGraphContainer[GeoVertex] = GraphUtils.createGridGeoGraph(50, 50, 20 * 20 + 1 + 10 * 10 + 1 + 15 * 15 + 1 + 3 * 3 + 1)

  val geoGraph = EagerGraphContainer
    .joinGraphs[GeoVertex, EagerGeoGraphContainer[GeoVertex]](List(geoGraph20, geoGraph10, geoGraph15, geoGraph3, geoGraph50), EagerGeoGraphContainer.apply)

  val graphGen: Gen[Unit] = Gen.unit("a bigger graph")

  /* tests */

  performance of "GraphUtils" in {
    measure method "getConnectedComponent" in {
      using(graphGen) in {
        _ ⇒ {
          GraphUtils.getConnectedComponent[GeoVertex, EagerGeoGraphContainer[GeoVertex]](geoGraph, EagerGeoGraphContainer.apply)
        }
      }
    }
  }

}

object GraphUtilsBetter {

  /**
    * A connected component is a maximal connected subgraph of G.
    * @return The maximal connected subgraph
    */
  def getConnectedComponent[V <: Vertex, G <: EagerGraphContainer[V]](graph: G, creator: (List[V]) ⇒ G): G = {
    creator(splitByConnectedGraph[V, G](graph, creator).max(Ordering.by[List[V], Int](list ⇒ list.size)))
  }

  def splitByConnectedGraph[V <: Vertex, G <: EagerGraphContainer[V]](graph: G, creator: (List[V]) ⇒ G): List[List[V]] = {

    @tailrec
    def findNotVisited(visits: List[V], graph: G, result: List[List[V]]): List[List[V]] = {
      graph.vertices filter (v ⇒ !visits.contains(v)) match {
        case list @ x :: xs ⇒
          val neighbours: List[V] = findNeighbours(list.head, graph)
          findNotVisited(neighbours, creator(list), neighbours :: result)
        case Nil ⇒ result
      }
    }

    findNotVisited(Nil, graph, Nil)
  }

  def findNeighbours[V <: Vertex](start: V, graph: GraphContainer[V]): List[V] = {
    def childrenNotVisited(vertex: V, visited: List[V]) =
      graph.neighbours(vertex) filter (x ⇒ !visited.contains(x)) toSet

    @tailrec
    def loop(stack: Set[V], visited: List[V]): List[V] = {
      if (stack isEmpty) visited
      else loop(childrenNotVisited(stack.head, visited) ++ stack.tail,
        stack.head :: visited)
    }
    loop(Set(start), Nil).distinct.reverse
  }

}
