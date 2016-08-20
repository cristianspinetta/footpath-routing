package mapdomain.math

import mapdomain.graph.Coordinate

case class Polygon(vertices: Vector[Coordinate]) {
  def hasPointInside(polygon: Polygon): Boolean =
    vertices.forall(coordinate ⇒ Polygon.isIn(coordinate, polygon))
}

object Polygon {
  def isIn(coordinate: Coordinate, polygon: Polygon): Boolean = {
    // http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
    val n = polygon.vertices.size
    var c: Boolean = false
    var i: Int = 0
    var j: Int = n - 1
    while (i < n) {
      {
        if ((
          (
            (polygon.vertices(i).latitude <= coordinate.latitude) &&
            (coordinate.latitude < polygon.vertices(j).latitude)) ||
            (
              (polygon.vertices(j).latitude <= coordinate.latitude) &&
              (coordinate.latitude < polygon.vertices(i).latitude))) &&
              (coordinate.longitude <
                (polygon.vertices(j).longitude - polygon.vertices(i).longitude) *
                (coordinate.latitude - polygon.vertices(i).latitude) /
                (polygon.vertices(j).latitude - polygon.vertices(i).latitude) +
                polygon.vertices(i).longitude)) c = !c

        j = i
        i += 1
      }
    }
    c
  }
}