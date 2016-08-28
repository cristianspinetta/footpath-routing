package mapdomain.utils

import mapdomain.graph._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

trait GraphUtils {

  def readableEdges(edges: List[Edge]): String =
    edgesToIds(edges) mkString " -> "

  def edgesToIds(edges: List[Edge]): List[Long] = edges.headOption.map(_.vertexStart).toList ::: edges.map(_.vertexEnd)

  def createGridGraphPrototype(rows: Int, columns: Int): GraphContainer[GraphVertex] = {

    var nodes = scala.collection.mutable.ListBuffer.empty[GraphVertex]
    var nodeNumber = 1

    for (row ← 1 to rows; column ← 1 to columns) {
      var neighbours = scala.collection.mutable.ListBuffer.empty[Int]

      val columnMod = column % columns
      if (columnMod == 1) {
        // First column
        neighbours += (nodeNumber + 1)
      } else if (columnMod == 0) {
        // Last column
        neighbours += (nodeNumber - 1)
      } else {
        // Between the fist and last column
        neighbours += (nodeNumber + 1)
        neighbours += (nodeNumber - 1)
      }

      val rowMod = row % rows
      if (rowMod == 1) {
        // First row
        neighbours += (nodeNumber + columns)
      } else if (rowMod == 0) {
        // Last row
        neighbours += (nodeNumber - columns)
      } else {
        // Between first and last row
        neighbours += (nodeNumber + columns)
        neighbours += (nodeNumber - columns)
      }

      nodes += GraphVertex.createWithEdges(nodeNumber, neighbours.toList)
      nodeNumber += 1
    }

    GraphContainer(nodes.toList)
  }

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

  def createGridGraph[T <: Vertex](rows: Int, columns: Int, offset: Int,
    vertexCreator: (Int, Int, Int, Iterable[NeighbourPartialCreation]) ⇒ T): GraphContainer[T] = {

    val nodes = ArrayBuffer.empty[T]
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
  def getConnectedComponent[T <: GeoVertex, G <: GraphContainer[T]](graph: G, creator: (List[T]) ⇒ G): G = {
    creator(splitByConnectedGraph(graph).max(Ordering.by[List[T], Int](list ⇒ list.size)))
  }

  def splitByConnectedGraph[T <: GeoVertex](graph: GraphContainer[T]): List[List[T]] = {

    @tailrec
    def findNotVisited(visits: List[T], graph: GraphContainer[T], result: List[List[T]]): List[List[T]] = {
      graph.vertices filter (v ⇒ !visits.contains(v)) match {
        case list @ x :: xs ⇒
          val neighbours: List[T] = findNeighbours(list.head, graph)
          findNotVisited(neighbours, GraphContainer(list), neighbours :: result)
        case Nil ⇒ result
      }
    }

    findNotVisited(Nil, graph, Nil)
  }

  def findNeighbours[T <: GeoVertex](start: T, graph: GraphContainer[T]): List[T] = {
    def childrenNotVisited(vertex: T, visited: List[T]) =
      vertex.neighbours(graph) filter (x ⇒ !visited.contains(x)) toSet

    @tailrec
    def loop(stack: Set[T], visited: List[T]): List[T] = {
      if (stack isEmpty) visited
      else loop(childrenNotVisited(stack.head, visited) ++ stack.tail,
        stack.head :: visited)
    }
    loop(Set(start), Nil).distinct.reverse
  }

  def isGraphConnected[T <: GeoVertex](graph: GraphContainer[T]): Boolean = {
    val neighbours = findNeighbours(graph.vertices.head, graph)
    graph.vertices forall (v ⇒ neighbours contains v)
  }

}

object GraphUtils extends GraphUtils
