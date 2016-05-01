package pathgenerator.graph

import java.lang.Math.{ pow, sqrt }

case class GraphContainer[V <: Vertex](vertices: List[V]) {

  def findVertex(id: Long): Option[V] = vertices.find(_.id == id) // TODO: replace by a DB query
}

trait Vertex {
  val id: Long
  val edges: List[Edge]

  def neighbours[V <: Vertex](graph: GraphContainer[V]): List[V] = edges.flatMap(edge ⇒ graph.findVertex(edge.vertexEnd) toList)
  def getEdgesFor(vertexId: Long): Option[Edge] = edges.find(_.vertexEnd == vertexId)
  def distanceToNeighbour(vertexId: Long): Double
}

case class GraphVertex(id: Long, edges: List[Edge]) extends Vertex {
  def distanceToNeighbour(vertexId: Long): Double = this.getEdgesFor(vertexId).map(_.distance).getOrElse(0L).toDouble
}

object GraphVertex {
  def createWithEdges(id: Long, edges: List[Int]): GraphVertex = GraphVertex(id, edges.map(Edge(id, _)))
  def removeEdge(source: GraphVertex, to: Long): GraphVertex = source.copy(edges = source.edges.filterNot(_.vertexEnd == to))
}

case class GeoVertex(id: Long, edges: List[Edge], coordinate: Coordinate) extends Vertex {
  def removeEdgeAt(to: Long): GeoVertex = this.copy(edges = edges.filterNot(_.vertexEnd == to))
  def distanceToNeighbour(vertexId: Long): Double = this.getEdgesFor(vertexId).map(_.distance).getOrElse(0L).toDouble // TODO report access by getOrElse
  def distanceTo(vertex: GeoVertex): Double = this.coordinate.distanceTo(vertex.coordinate)
  def removeEdge(source: GeoVertex, to: Long): GeoVertex = source.copy(edges = source.edges.filterNot(_.vertexEnd == to))
}
object GeoVertex {
  def createWithEdges(id: Long, edges: List[Int], coordinate: Coordinate): GeoVertex =
    GeoVertex(id, edges.map(Edge(id, _)), coordinate)
}

case class Coordinate(latitude: Double, longitude: Double) {
  val _latitudeInRadians: Double = latitude.toRadians
  val _longitudeInRadians: Double = longitude.toRadians

  def distanceTo(coordinate: Coordinate): Double = {
    sqrt(pow(_latitudeInRadians - coordinate.latitude.toRadians, 2) +
      pow(_longitudeInRadians - coordinate.longitude.toRadians, 2))
  }
}

case class Edge(vertexStart: Long, vertexEnd: Long, distance: Long = 10, directed: Boolean = true)

trait Heuristic[V <: Vertex] {
  def apply(vertex: V): Double
}

case class TrivialHeuristic[V <: Vertex]() extends Heuristic[V] {
  override def apply(vertex: V): Double = 0
}

case class GeoHeuristic(referenceVertex: GeoVertex) extends Heuristic[GeoVertex] {
  override def apply(vertex: GeoVertex): Double = referenceVertex.coordinate.distanceTo(vertex.coordinate)
}
