package pathgenerator.core

import pathgenerator.graph._
import pathgenerator.testkit.BaseAStarSpec
import pathgenerator.utils.GraphUtils
import org.scalatest.{ Matchers, WordSpec }

import scala.util.Try

class AStarSpec extends WordSpec with BaseAStarSpec with Matchers with GraphUtils {

  "The A* Algorithm with the trivial F Heuristic { Y = 0, ∀ X ∈ ℝ }" when {

    val aStarWithTrivialHeuristic = AStar[GraphVertex](TrivialHeuristic()) _

    "it runs on the Graph A" should {
      "resolve correctly the path between 1 and 4" in {
        val graph: GraphContainer[GraphVertex] = abstractGraphPrototype

        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 4L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result = aStar.search

        val expectedEdges = buildEdges(1, List(2, 3, 4))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }

      "resolve correctly the path between 1 and 18" in {
        val graph: GraphContainer[GraphVertex] = abstractGraphPrototype

        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result: Try[List[Edge]] = aStar.search

        val expectedEdges = buildEdges(1, List(6, 5, 11, 10, 13, 18))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }

      "resolve correctly the path between 1 and 19" in {
        val graph: GraphContainer[GraphVertex] = abstractGraphPrototype

        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result: Try[List[Edge]] = aStar.search

        val expectedEdges = buildEdges(1, List(6, 5, 11, 10, 13, 18, 19))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }
    }

    "it runs on the Graph A without edge 10-13" should {

      val graph: GraphContainer[GraphVertex] = abstractGraphPrototype.copy(vertices =
        abstractGraphPrototype.vertices.map {
          case vertex @ GraphVertex(10, edges) ⇒ GraphVertex.removeEdge(vertex, 13)
          case vertex                          ⇒ vertex
        })

      "resolve correctly the path between 1 and 18" in {
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result = aStar.search

        val expectedEdges = buildEdges(1, List(2, 3, 4, 8, 9, 14, 18))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }

      "resolve correctly the path between 1 and 19" in {
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result = aStar.search

        val expectedEdges = buildEdges(1, List(2, 3, 4, 8, 9, 14, 18, 19))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }
    }

    "it runs on the Graph A neither with edge 10-13 nor 14-18" should {

      val graph: GraphContainer[GraphVertex] = abstractGraphPrototype.copy(vertices =
        abstractGraphPrototype.vertices.map {
          case vertex @ GraphVertex(10, edges) ⇒ GraphVertex.removeEdge(vertex, 13)
          case vertex @ GraphVertex(14, edges) ⇒ GraphVertex.removeEdge(vertex, 18)
          case vertex                          ⇒ vertex
        })

      "resolve correctly the path between 1 and 18" in {
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 18L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result = aStar.search

        val expectedEdges = buildEdges(1, List(6, 5, 11, 10, 12, 15, 16, 17, 19, 18))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }

      "resolve correctly the path between 1 and 19" in {
        val source: GraphVertex = graph.vertices.find(_.id == 1L).get
        val target: GraphVertex = graph.vertices.find(_.id == 19L).get

        val aStar = aStarWithTrivialHeuristic(graph, source, target)

        val result = aStar.search

        val expectedEdges = buildEdges(1, List(6, 5, 11, 10, 12, 15, 16, 17, 19))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }
    }
  }

  "The A* Algorithm with the Geographical Heuristic Function" when {

    "it runs on the Graph A" should {
      "resolve correctly the path between 1 and 4" in {
        val graph: GraphContainer[GeoVertex] = geoGraphPrototype

        val source: GeoVertex = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex = graph.vertices.find(_.id == 4L).get

        val aStar = AStar[GeoVertex](GeoHeuristic(source))(graph, source, target)

        val result = aStar.search

        val expectedEdges = buildEdges(1, List(2, 3, 4))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }

      "resolve correctly the path between 1 and 18" in {
        val graph: GraphContainer[GeoVertex] = geoGraphPrototype

        val source: GeoVertex = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex = graph.vertices.find(_.id == 18L).get

        val aStar = AStar[GeoVertex](GeoHeuristic(source))(graph, source, target)

        val result: Try[List[Edge]] = aStar.search

        val expectedEdges = buildEdges(1, List(6, 5, 11, 10, 13, 18))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }

      "resolve correctly the path between 1 and 19" in {
        val graph: GraphContainer[GeoVertex] = geoGraphPrototype

        val source: GeoVertex = graph.vertices.find(_.id == 1L).get
        val target: GeoVertex = graph.vertices.find(_.id == 19L).get

        val aStar = AStar[GeoVertex](GeoHeuristic(source))(graph, source, target)

        val result: Try[List[Edge]] = aStar.search

        val expectedEdges = buildEdges(1, List(6, 5, 11, 10, 13, 18, 19))

        result.isSuccess should be(true)

        result.foreach(r ⇒
          withClue(pathFailureMessage(r, expectedEdges)) {
            r should be(expectedEdges)
          })
      }
    }
  }

  private def buildEdges(startVertexId: Long, edgeIds: List[Long]): List[Edge] = edgeIds
    .foldLeft((startVertexId, List.empty[Edge])) {
      case ((cameFrom, partialEdges), nextVertex) ⇒
        (nextVertex, partialEdges :+ Edge(cameFrom, nextVertex))
    }._2

  private def pathFailureMessage(result: List[Edge], expectedEdges: List[Edge]): String = {
    s"The expected edges: ${printReadableEdges(expectedEdges)}. The result: ${printReadableEdges(result)}"
  }

  private def assertionEdges(result: List[Edge], expectedEdges: List[Edge]): Unit = {
    assert(expectedEdges == result,
      s"The expected edges: ${printReadableEdges(expectedEdges)}. The result: ${printReadableEdges(result)}")
  }
}
