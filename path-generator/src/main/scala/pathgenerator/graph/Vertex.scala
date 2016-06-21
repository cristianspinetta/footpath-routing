package pathgenerator.graph

import scala.math._

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

  def sortEdgesByAngle(from: GeoVertex)(implicit graph: GraphContainer[GeoVertex]): List[GeoEdge] = {
    from.edges.sortBy(edge ⇒ { //TODO: Ver de mejorar el ordenamiento
      from.coordinate.angleTo(graph.findVertex(edge.vertexEnd).get.coordinate)
    })(Ordering[Double])
  }
}

case class Coordinate(latitude: Double, longitude: Double) {
  private val φ1: Double = latitude.toRadians

  /**
   * The distance between this point and the given one in meters.
   * @param to: point to where the distance is calculated.
   * @return the distance in meters.
   */
  def distanceTo(to: Coordinate): Double = {
    val φ2 = toRadians(to.latitude)
    val Δφ = toRadians(to.latitude - latitude)
    val Δλ = toRadians(to.longitude - longitude)
    val a = pow(sin(Δφ / 2), 2) + cos(φ1) * cos(φ2) * pow(sin(Δλ / 2), 2)
    2 * atan2(sqrt(a), sqrt(1 - a)) * Coordinate.radius
  }

  /**
   * The angle from this point to the given one.
   * The latitude of this point is the adjacent side, the longitude of the given point
   * is the opposing side and the line from this point to the given one is the hypotenuse.
   * @param to: point to where the angle is calculated.
   * @return the angle in radians.
   */
  def angleTo(to: Coordinate): Double = {
    val fromLat: Double = latitude
    val fromLng: Double = longitude
    val toLat: Double = to.latitude
    val toLng: Double = to.longitude

    val diffLat = abs(fromLat - toLat)
    val diffLng = abs(fromLng - toLng)

    (diffLat, diffLng) match {
      case (0, 0) ⇒ 0
      case (_, 0) ⇒
        if (toLat > fromLat) Pi / 2
        else (3 * Pi) / 2
      case (0, _) ⇒
        if (toLng >= fromLng) 0
        else Pi
      case _ ⇒
        val arcTan = atan(diffLat / diffLng)
        if (fromLat > toLat && fromLng >= toLng) // 3rd quadrant
          arcTan + Pi
        else if (fromLat > toLat && fromLng < toLng) // 4th quadrant
          arcTan + (3 * Pi) / 2
        else if (fromLat < toLat && fromLng > toLng) // 2nd quadrant
          arcTan + Pi / 2
        else arcTan // 1st quadrant
    }
  }
}

object Coordinate {
  val radius: Double = 6.371 // meters
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
