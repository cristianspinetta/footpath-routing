package pathgenerator.graph

import java.lang.Math._

trait Node {
  val id: Long
  val edges: List[Edge]

  def neighbours[V <: Node](graph: GraphContainer[V]): List[V] = edges.flatMap(edge ⇒ graph.findNode(edge.nodeEnd) toList)
  def getEdgesFor(nodeId: Long): Option[Edge] = edges.find(_.nodeEnd == nodeId)
  def distanceToNeighbour(nodeId: Long): Double
}

case class GraphNode(id: Long, edges: List[Edge]) extends Node {
  def distanceToNeighbour(nodeId: Long): Double = this.getEdgesFor(nodeId).map(_.distance).getOrElse(0)
}

object GraphNode {
  def createWithEdges(id: Long, edges: List[Int]): GraphNode = GraphNode(id, edges.map(GraphEdge(id, _)))
  def removeEdge(source: GraphNode, to: Long): GraphNode = source.copy(edges = source.edges.filterNot(_.nodeEnd == to))
}

case class GeoNode(id: Long, edges: List[GeoEdge], coordinate: Coordinate) extends Node {
  def removeEdgeAt(to: Long): GeoNode = this.copy(edges = edges.filterNot(_.nodeEnd == to))
  def distanceToNeighbour(nodeId: Long): Double = this.getEdgesFor(nodeId).map(_.distance).getOrElse(0) // TODO report access by getOrElse
  def distanceTo(node: GeoNode): Double = this.coordinate.distanceTo(node.coordinate)
  def removeEdge(source: GeoNode, to: Long): GeoNode = source.copy(edges = source.edges.filterNot(_.nodeEnd == to))
}
object GeoNode {
  def createWithEdges(id: Long, edges: List[(Long, Coordinate)], coordinate: Coordinate): GeoNode = {
    val edge: (Long, Double) ⇒ GeoEdge = GeoEdge(id) _

    GeoNode(id, edges.map {
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
  val nodeStart: Long
  val nodeEnd: Long
  val distance: Double
  val directed: Boolean = true
}

case class GraphEdge(nodeStart: Long, nodeEnd: Long, distance: Double = 10, override val directed: Boolean = true) extends Edge

case class GeoEdge(nodeStart: Long, nodeEnd: Long, distance: Double, override val directed: Boolean = true) extends Edge

object GeoEdge {

  def apply(nodeStart: Long)(nodeEnd: Long, distance: Double): GeoEdge = new GeoEdge(nodeStart, nodeEnd, distance)
}
