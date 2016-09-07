package mapdomain.utils

import mapdomain.graph._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait GraphUtils {

  def readableEdges(edges: List[Edge]): String = edgesToIds(edges) mkString " -> "

  def edgesToIds(edges: List[Edge]): List[Long] = edges.headOption.map(_.vertexStartId).toList ::: edges.map(_.vertexEndId)

  def createGridGraphGraph(rows: Int, columns: Int, offset: Int = 0): GraphContainer[GraphVertex] = {
    val vertexCreator = (id: Int, row: Int, column: Int, neighbours: Iterable[NeighbourPartialCreation]) ⇒
      GraphVertex.createWithEdges(id, neighbours.toList.map(npc ⇒ npc.id))
    createGridGraph(rows, columns, offset, vertexCreator)
  }

  def createGridGeoGraph(rows: Int, columns: Int, offset: Int = 0): GraphContainer[GeoVertex] = {
    val vertexCreator = (id: Int, row: Int, column: Int, neighbours: Iterable[NeighbourPartialCreation]) ⇒
      GeoVertex.createWithEdges(id, neighbours.toList.map(npc ⇒ (npc.id.toLong, npc.coordinate)), Coordinate(row, column))
    createGridGraph(rows, columns, offset, vertexCreator)
  }

  case class NeighbourPartialCreation(id: Int, coordinate: Coordinate)

  def createGridGraph[V <: Vertex](rows: Int, columns: Int, offset: Int,
    vertexCreator: (Int, Int, Int, Iterable[NeighbourPartialCreation]) ⇒ V): GraphContainer[V] = {

    val nodes = ArrayBuffer.empty[V]
    var nodeNumber = offset

    for (row ← 1 to rows; column ← 1 to columns) {
      var neighbours = ArrayBuffer.empty[NeighbourPartialCreation]

      val columnMod = column % columns
      if (columnMod == 1) {
        // First column
        neighbours += NeighbourPartialCreation(nodeNumber + 1, Coordinate(row, nodeNumber + 1))
      } else if (columnMod == 0) {
        // Last column
        neighbours += NeighbourPartialCreation(nodeNumber - 1, Coordinate(row, nodeNumber - 1))
      } else {
        // Between the fist and last column
        neighbours += NeighbourPartialCreation(nodeNumber + 1, Coordinate(row, nodeNumber + 1))
        neighbours += NeighbourPartialCreation(nodeNumber - 1, Coordinate(row, nodeNumber - 1))
      }

      val rowMod = row % rows
      if (rowMod == 1) {
        // First row
        neighbours += NeighbourPartialCreation(nodeNumber + columns, Coordinate(nodeNumber + columns, column))
      } else if (rowMod == 0) {
        // Last row
        neighbours += NeighbourPartialCreation(nodeNumber - columns, Coordinate(nodeNumber - columns, column))
      } else {
        // Between first and last row
        neighbours += NeighbourPartialCreation(nodeNumber + columns, Coordinate(nodeNumber + columns, column))
        neighbours += NeighbourPartialCreation(nodeNumber - columns, Coordinate(nodeNumber - columns, column))
      }

      nodes += vertexCreator(nodeNumber, row, column, neighbours)
      nodeNumber += 1
    }

    GraphContainer(nodes.toList)
  }

  /**
   * A connected component is a maximal connected subgraph of G.
   * @return The maximal connected subgraph
   */
  def getConnectedComponent[V <: Vertex, G <: GraphContainer[V]](graph: G, creator: (List[V]) ⇒ G): G = {
    creator(splitByConnectedGraph(graph).max(Ordering.by[List[V], Int](list ⇒ list.size)))
  }

  def splitByConnectedGraph[V <: Vertex](graph: GraphContainer[V]): List[List[V]] = {

    @tailrec
    def findNotVisited(visits: List[V], graph: GraphContainer[V], result: List[List[V]]): List[List[V]] = {
      graph.vertices filter (v ⇒ !visits.contains(v)) match {
        case list @ x :: xs ⇒
          val neighbours: List[V] = findNeighbours(list.head, graph)
          findNotVisited(neighbours, GraphContainer(list), neighbours :: result)
        case Nil ⇒ result
      }
    }

    findNotVisited(Nil, graph, Nil)
  }

  def findNeighbours[V <: Vertex](start: V, graph: GraphContainer[V]): List[V] = {
    def childrenNotVisited(vertex: V, visited: List[V]) =
      vertex.neighbours(graph) filter (x ⇒ !visited.contains(x)) toSet

    @tailrec
    def loop(stack: Set[V], visited: List[V]): List[V] = {
      if (stack isEmpty) visited
      else loop(childrenNotVisited(stack.head, visited) ++ stack.tail,
        stack.head :: visited)
    }
    loop(Set(start), Nil).distinct.reverse
  }

  def isGraphConnected[V <: Vertex](graph: GraphContainer[V]): Boolean = {
    val neighbours = findNeighbours(graph.vertices.head, graph)
    graph.vertices forall (v ⇒ neighbours contains v)
  }

}

object GraphUtils extends GraphUtils
