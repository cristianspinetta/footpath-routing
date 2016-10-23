package pathgenerator.core

import mapdomain.graph._
import pathgenerator.graph._
import pathgenerator.testkit.BaseAStarSpec
import mapdomain.utils.GraphUtils
import org.scalatest.{ Matchers, WordSpec }

import scala.util.Try

class AStarSpec extends WordSpec with BaseAStarSpec with Matchers with GraphUtils {

  "The A* Algorithm with the trivial F Heuristic { Y = 0, ∀ X ∈ ℝ }" when {

    val aStarWithTrivialHeuristic = AStar[GraphEdge, GraphVertex[GraphEdge], TrivialHeuristic[GraphEdge, GraphVertex[GraphEdge]], GeoGCost[GraphEdge, GraphVertex[GraphEdge]]](TrivialHeuristic(), GeoGCost()) _

    "it runs on the Graph A" should {

      val graph: InMemoryGraphContainer[GraphEdge, GraphVertex[GraphEdge]] = abstractGraphPrototype

      "resolve correctly the path between 1 and 4" in {

        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 4L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result = aStar.search

        val expectedPath: List[Long] = List(1, 2, 3, 4)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path between 1 and 18" in {

        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result: Try[List[Edge]] = aStar.search

        val expectedPath: List[Long] = List(1, 6, 5, 11, 10, 13, 18)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path between 1 and 19" in {

        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result: Try[List[Edge]] = aStar.search

        val expectedPath: List[Long] = List(1, 6, 5, 11, 10, 13, 18, 19)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path from 1 to 19 or 14" in {

        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target1: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 19L).get
        val target2: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 14L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target1, target2))

        val result: Try[List[Edge]] = aStar.search

        val expectedPath: List[Long] = List(1, 2, 3, 4, 8, 9, 14)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path from 1 to 19 or 14 or 7" in {

        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target1: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 19L).get
        val target2: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 14L).get
        val target3: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 7L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target1, target2, target3))

        val result: Try[List[Edge]] = aStar.search

        val expectedPath: List[Long] = List(1, 2, 3, 7)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }
    }

    "it runs on the Graph A without edge 10-13" should {

      val graph: InMemoryGraphContainer[GraphEdge, GraphVertex[GraphEdge]] = InMemoryGraphContainer(vertices =
        abstractGraphPrototype.vertices.map {
          case node @ GraphVertex(10, edges) ⇒ GraphVertex.removeEdge(node, 13)
          case node                          ⇒ node
        })

      "resolve correctly the path between 1 and 18" in {
        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result = aStar.search

        val expectedPath: List[Long] = List(1, 2, 3, 4, 8, 9, 14, 18)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path between 1 and 19" in {
        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result = aStar.search

        val expectedPath: List[Long] = List(1, 2, 3, 4, 8, 9, 14, 18, 19)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }
    }

    "it runs on the Graph A neither without edge 10-13 nor 14-18" should {

      val graph: InMemoryGraphContainer[GraphEdge, GraphVertex[GraphEdge]] = InMemoryGraphContainer(vertices =
        abstractGraphPrototype.vertices.map {
          case node @ GraphVertex(10, edges) ⇒ GraphVertex.removeEdge(node, 13)
          case node @ GraphVertex(14, edges) ⇒ GraphVertex.removeEdge(node, 18)
          case node                          ⇒ node
        })

      "resolve correctly the path between 1 and 18" in {
        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result = aStar.search

        val expectedPath: List[Long] = List(1, 6, 5, 11, 10, 12, 15, 16, 17, 19, 18)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path between 1 and 19" in {
        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result = aStar.search

        val expectedPath: List[Long] = List(1, 6, 5, 11, 10, 12, 15, 16, 17, 19)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }
    }

    "it runs on Grid Graph with 20000 vertices" should {
      "resolve correctly the path between 1 and 16888" in {
        val graph: InMemoryGraphContainer[GraphEdge, GraphVertex[GraphEdge]] = createGridGraphGraph(100, 200)

        val source: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex[GraphEdge] = graph.vertices.find(_.id == 16888L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, Seq(target))

        val result = aStar.search

        result.isSuccess should be(true)
      }
    }
  }

  "The A* Algorithm with the Geographical Heuristic Function" when {

    "it runs on the Graph A" should {
      "resolve correctly the path between 1 and 4" in {
        val graph: InMemoryGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = geoGraphPrototype

        val source: GeoVertex[GeoEdge] = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex[GeoEdge] = graph.vertices.find(_.id == 4L).get

        val aStar = AStar[GeoEdge, GeoVertex[GeoEdge], GeoHeuristic[GeoEdge, GeoVertex[GeoEdge]], GeoGCost[GeoEdge, GeoVertex[GeoEdge]]](GeoHeuristic(source), GeoGCost())(graph, source, target)

        val result = aStar.search

        val expectedPath: List[Long] = List(1, 2, 4)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path between 1 and 13" in {
        val graph: InMemoryGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = geoGraphPrototype

        val source: GeoVertex[GeoEdge] = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex[GeoEdge] = graph.vertices.find(_.id == 13L).get

        val aStar = AStar[GeoEdge, GeoVertex[GeoEdge], GeoHeuristic[GeoEdge, GeoVertex[GeoEdge]], GeoGCost[GeoEdge, GeoVertex[GeoEdge]]](GeoHeuristic(source), GeoGCost())(graph, source, target)

        val result: Try[List[Edge]] = aStar.search

        val expectedPath: List[Long] = List(1, 2, 4, 7, 8, 9, 14, 13)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }

      "resolve correctly the path between 1 and 12" in {
        val graph: InMemoryGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = geoGraphPrototype

        val source: GeoVertex[GeoEdge] = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex[GeoEdge] = graph.vertices.find(_.id == 12L).get

        val aStar = AStar[GeoEdge, GeoVertex[GeoEdge], GeoHeuristic[GeoEdge, GeoVertex[GeoEdge]], GeoGCost[GeoEdge, GeoVertex[GeoEdge]]](GeoHeuristic(source), GeoGCost())(graph, source, target)

        val result: Try[List[Edge]] = aStar.search

        val expectedPath: List[Long] = List(1, 6, 5, 11, 12)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }
    }
  }

  private def pathFailureMessage(result: List[Long], expectedPath: List[Long]): String = {
    s"The expected edges: ${expectedPath mkString " -> "}. The result: ${result mkString " -> "}"
  }
}
