package pathgenerator.graph

import java.lang.Math._

trait Vertex {
  val id: Long
  val edges: List[Edge]

  def neighbours[V <: Vertex](graph: GraphContainer[V]): List[V] = edges.flatMap(edge ⇒ graph.findVertex(edge.vertexEnd) toList)
  def getEdgesFor(vertexId: Long): Option[Edge] = edges.find(_.vertexEnd == vertexId)
  def distanceToNeighbour(vertexId: Long): Double
}

case class GraphVertex(id: Long, edges: List[Edge]) extends Vertex {
  def distanceToNeighbour(vertexId: Long): Double = this.getEdgesFor(vertexId).map(_.distance).getOrElse(0)
}

object GraphVertex {
  def createWithEdges(id: Long, edges: List[Int]): GraphVertex = GraphVertex(id, edges.map(GraphEdge(id, _)))
  def removeEdge(source: GraphVertex, to: Long): GraphVertex = source.copy(edges = source.edges.filterNot(_.vertexEnd == to))
}

class GeoVertex(override val id: Long, override val edges: List[GeoEdge], val coordinate: Coordinate) extends Vertex {
  //  def removeEdgeAt(to: Long): GeoVertex = this.copy(edges = edges.filterNot(_.vertexEnd == to))
  def distanceToNeighbour(vertexId: Long): Double = this.getEdgesFor(vertexId).map(_.distance).getOrElse(0) // TODO report access by getOrElse
  def distanceTo(vertex: GeoVertex): Double = this.coordinate.distanceTo(vertex.coordinate)
  //  def removeEdge(source: GeoVertex, to: Long): GeoVertex = source.copy(edges = source.edges.filterNot(_.vertexEnd == to))
}
object GeoVertex {
  def createWithEdges(id: Long, edges: List[(Long, Coordinate)], coordinate: Coordinate): GeoVertex = {
    val edge: (Long, Double) ⇒ GeoEdge = GeoEdge(id) _

    new GeoVertex(id, edges.map {
      case (neighbourId, neighbourCoordinate) ⇒
        edge(neighbourId, neighbourCoordinate.distanceTo(coordinate))
    }, coordinate)
  }
}

case class Coordinate(latitude: Double, longitude: Double) {
  val _latitudeInRadians: Double = latitude.toRadians
  val _longitudeInRadians: Double = longitude.toRadians

  def distanceTo(coordinate: Coordinate): Double = {
    sqrt(pow(_latitudeInRadians - coordinate.latitude.toRadians, 2) +
      pow(_longitudeInRadians - coordinate.longitude.toRadians, 2))
  }
}

trait Edge {
  val vertexStart: Long
  val vertexEnd: Long
  val distance: Double
  val directed: Boolean = true
}

case class GraphEdge(vertexStart: Long, vertexEnd: Long, distance: Double = 10, override val directed: Boolean = true) extends Edge

class GeoEdge(override val vertexStart: Long, override val vertexEnd: Long, override val distance: Double, override val directed: Boolean = true) extends Edge

object GeoEdge {

  def apply(vertexStart: Long, vertexEnd: Long, distance: Double, directed: Boolean = true): GeoEdge = new GeoEdge(vertexStart, vertexEnd, distance, directed)

  def apply(vertexStart: Long)(vertexEnd: Long, distance: Double): GeoEdge = new GeoEdge(vertexStart, vertexEnd, distance)
}
