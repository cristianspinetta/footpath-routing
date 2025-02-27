package mapdomain.utils

import mapdomain.graph._

import scala.annotation.tailrec
import scala.collection.Map
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import scala.language.existentials
import scala.math.Ordering

trait GraphUtils {

  def readableEdges(edges: List[Edge]): String = edgesToIds(edges) mkString " -> "

  def edgesToIds(edges: List[Edge]): List[Long] = edges.headOption.map(_.vertexStartId).toList ::: edges.map(_.vertexEndId)
  def edgeReferencesToIds[E <: Edge, V <: Vertex[E]](edges: List[EdgeReference[E, V]]): List[Long] = edges.headOption.map(_.vertexStart.id).toList ::: edges.map(_.vertexEnd.id)

  def createGridGraphGraph(rows: Int, columns: Int, offset: Int = 0): InMemoryGraphContainer[GraphEdge, GraphVertex[GraphEdge]] = {
    val vertexCreator: (Int, Int, Int, Iterable[NeighbourPartialCreation]) => GraphVertex[GraphEdge] = (id: Int, row: Int, column: Int, neighbours: Iterable[NeighbourPartialCreation]) ⇒
      GraphVertex.createWithEdges(id, neighbours.toList.map(npc ⇒ npc.id))
    createGridGraph[GraphEdge, GraphVertex[GraphEdge], InMemoryGraphContainer[GraphEdge, GraphVertex[GraphEdge]]](rows, columns, offset, vertexCreator, InMemoryGraphContainer.apply)
  }

  def createGridGeoGraph(rows: Int, columns: Int, offset: Int = 0): InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]] = {
    val vertexCreator = (id: Int, row: Int, column: Int, neighbours: Iterable[NeighbourPartialCreation]) ⇒
      GeoVertex.createWithEdges(id, neighbours.toList.map(npc ⇒ (npc.id.toLong, npc.coordinate)), Coordinate(row, column))
    createGridGraph[GeoEdge, GeoVertex[GeoEdge], InMemoryGeoGraphContainer[GeoEdge, GeoVertex[GeoEdge]]](rows, columns, offset, vertexCreator, InMemoryGeoGraphContainer.apply)
  }

  case class NeighbourPartialCreation(id: Int, coordinate: Coordinate)

  def createGridGraph[E <: Edge, V <: Vertex[E], G <: InMemoryGraphContainer[E, V]](rows: Int, columns: Int, offset: Int,
                                                                   vertexCreator: (Int, Int, Int, Iterable[NeighbourPartialCreation]) ⇒ V, graphCreator: (List[V]) ⇒ G): G = {

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

    graphCreator(nodes.toList)
  }

  /**
    * A connected component is a maximal connected subgraph of G.
    * @return The maximal connected subgraph
    */
  def getConnectedComponent[E <: Edge, V <: Vertex[E], G <: InMemoryGraphContainer[E, V]](graph: G, creator: (List[V]) ⇒ G): G = {
    creator(splitByConnectedGraph[E, V, G](graph, creator).max(Ordering.by[List[V], Int](list ⇒ list.size)))
  }

  def splitByConnectedGraph[E <: Edge, V <: Vertex[E], G <: InMemoryGraphContainer[E, V]](graph: G, creator: (List[V]) ⇒ G): List[List[V]] = {

    @tailrec
    def findNotVisited(visits: TrieMap[Long, V], graph: G, result: List[List[V]]): List[List[V]] = {
      graph.vertices filter (v ⇒ !visits.contains(v.id)) match {
        case list @ x :: xs ⇒
          val neighbours: TrieMap[Long, V] = findNeighbours(list.head, graph)
          findNotVisited(neighbours, creator(list), neighbours.values.toList :: result)
        case Nil ⇒ result
      }
    }

    findNotVisited(TrieMap.empty, graph, Nil)
  }

  private def neighbours[E <: Edge, V <: Vertex[E]](vertex: V, graph: GraphContainer[E, V]): List[V] = {
    val neighbourIds: List[Long] = vertex.edges.map(edge ⇒ if (edge.vertexStartId == vertex.id) edge.vertexEndId else edge.vertexStartId)
    neighbourIds.flatMap(id ⇒ graph.findVertex(id) toList)
  }

  def findNeighbours[E <: Edge, V <: Vertex[E]](start: V, graph: GraphContainer[E, V]): TrieMap[Long, V] = {
    def childrenNotVisited(vertex: V, visited: TrieMap[Long, V]): TrieMap[Long, V] = {
      val notVisited = new TrieMap[Long, V]()
      neighbours(vertex, graph).foreach((x: V) ⇒ if (!visited.contains(x.id)) notVisited += (x.id -> x))
      notVisited
    }

    @tailrec
    def loop(stack: TrieMap[Long, V], visited: TrieMap[Long, V]): TrieMap[Long, V] = {
      if (stack isEmpty) visited
      else loop(childrenNotVisited(stack.head._2, visited) ++= stack.tail,
        visited += stack.head)
    }
    loop(new TrieMap[Long, V]() += (start.id -> start), TrieMap.empty)
  }

  def isGraphConnected[E <: Edge, V <: Vertex[E]](graph: InMemoryGraphContainer[E, V]): Boolean = {
    val neighbours: TrieMap[Long, V] = findNeighbours(graph.vertices.head, graph)
    graph.vertices forall (v ⇒ neighbours contains v.id)
  }

  def verticesToMap[E <: Edge, V <: Vertex[E]](vertices: Traversable[V]): Map[Long, V] = {
    val map = new TrieMap[Long, V]()
    vertices.foreach(v => map += (v.id -> v))
    map.readOnlySnapshot()
  }

}

object GraphUtils extends GraphUtils
