package mapdomain.graph

import scala.math._

case class Coordinate(latitude: Double, longitude: Double) {
  private val φ1: Double = latitude.toRadians

  /**
   * The distance between this point and the given one in meters.
   *
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
   *
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

  override def toString: String = s"Coordinate(Lat: $latitude, Lng: $longitude)"
}

object Coordinate {
  val radius: Double = 6.371 // meters
}
