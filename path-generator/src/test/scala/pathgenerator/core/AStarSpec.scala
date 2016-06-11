package pathgenerator.core

import pathgenerator.graph._
import pathgenerator.testkit.BaseAStarSpec
import pathgenerator.utils.GraphUtils
import org.scalatest.{ Matchers, WordSpec }

import scala.util.Try

class AStarSpec extends WordSpec with BaseAStarSpec with Matchers with GraphUtils {

  "The A* Algorithm with the trivial F Heuristic { Y = 0, ∀ X ∈ ℝ }" when {

    val aStarWithTrivialHeuristic = AStar[GraphVertex, TrivialHeuristic[GraphVertex]](TrivialHeuristic[GraphVertex]()) _

    "it runs on the Graph A" should {
      "resolve correctly the path between 1 and 4" in {
        val graph: GraphContainer[GraphVertex] = abstractGraphPrototype

        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 4L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

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
        val graph: GraphContainer[GraphVertex] = abstractGraphPrototype

        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

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
        val graph: GraphContainer[GraphVertex] = abstractGraphPrototype

        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result: Try[List[Edge]] = aStar.search

        val expectedPath: List[Long] = List(1, 6, 5, 11, 10, 13, 18, 19)

        result.isSuccess should be(true)

        val edges = result.get

        val edgesIds: List[Long] = edgesToIds(edges)

        withClue(pathFailureMessage(edgesIds, expectedPath)) {
          edgesIds should be(expectedPath)
        }
      }
    }

    "it runs on the Graph A without edge 10-13" should {

      val graph: GraphContainer[GraphVertex] = abstractGraphPrototype.copy(vertices =
        abstractGraphPrototype.vertices.map {
          case node @ GraphVertex(10, edges) ⇒ GraphVertex.removeEdge(node, 13)
          case node                          ⇒ node
        })

      "resolve correctly the path between 1 and 18" in {
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

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
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

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

    "it runs on the Graph A neither with edge 10-13 nor 14-18" should {

      val graph: GraphContainer[GraphVertex] = abstractGraphPrototype.copy(vertices =
        abstractGraphPrototype.vertices.map {
          case node @ GraphVertex(10, edges) ⇒ GraphVertex.removeEdge(node, 13)
          case node @ GraphVertex(14, edges) ⇒ GraphVertex.removeEdge(node, 18)
          case node                          ⇒ node
        })

      "resolve correctly the path between 1 and 18" in {
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

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
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

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
        val graph: GraphContainer[GraphVertex] = createGridGraphPrototype(100, 200)

        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 16888L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result = aStar.search

        result.isSuccess should be(true)
      }
    }
  }

  private def edgesToIds(edges: List[Edge]): List[Long] = {
    edges.headOption.map(_.vertexStart).toList ::: edges.map(_.vertexEnd)
  }

  "The A* Algorithm with the Geographical Heuristic Function" when {

    "it runs on the Graph A" should {
      "resolve correctly the path between 1 and 4" in {
        val graph: GraphContainer[GeoVertex] = geoGraphPrototype

        val source: GeoVertex = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex = graph.vertices.find(_.id == 4L).get

        val aStar = AStar[GeoVertex, GeoHeuristic[GeoVertex]](GeoHeuristic(source))(graph, source, target)

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
        val graph: GraphContainer[GeoVertex] = geoGraphPrototype

        val source: GeoVertex = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex = graph.vertices.find(_.id == 13L).get

        val aStar = AStar[GeoVertex, GeoHeuristic[GeoVertex]](GeoHeuristic(source))(graph, source, target)

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
        val graph: GraphContainer[GeoVertex] = geoGraphPrototype

        val source: GeoVertex = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex = graph.vertices.find(_.id == 12L).get

        val aStar = AStar[GeoVertex, GeoHeuristic[GeoVertex]](GeoHeuristic(source))(graph, source, target)

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
