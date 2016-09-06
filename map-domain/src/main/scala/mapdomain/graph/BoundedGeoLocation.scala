package mapdomain.graph

import scala.math._

case class BoundedGeoLocation(min: Coordinate, max: Coordinate) {
  val meridian180WithinDistance = min.radLongitude > max.radLongitude
}

object BoundedGeoLocation {

  def boundByDistance(coordinate: Coordinate, distance: Double, radius: Double = Coordinate.radius) = {

    if (radius < 0d || distance < 0d)
      throw new IllegalArgumentException()

    // angular distance in radians on a great circle
    val radDist = distance / radius

    val minLatCandidate = coordinate.radLatitude - radDist
    val maxLatCandidate = coordinate.radLatitude + radDist

    val MIN_LAT = (-90d).toRadians // -PI/2
    val MAX_LAT = 90d.toRadians //  PI/2
    val MIN_LON = (-180d).toRadians // -PI
    val MAX_LON = 180d.toRadians //  PI

    val (maxLat, minLat, minLng, maxLng) = if (minLatCandidate > MIN_LAT && maxLatCandidate < MAX_LAT) {
      val deltaLon = asin(sin(radDist) / cos(coordinate.radLatitude))

      val minLngCandidate = coordinate.radLongitude - deltaLon
      val finalMinLng = if (minLngCandidate < MIN_LON) minLngCandidate + 2d * Pi else minLngCandidate

      val maxLngCandidate = coordinate.radLongitude + deltaLon
      val finalMaxLng = if (maxLngCandidate > MAX_LON) maxLngCandidate - 2d * Pi else maxLngCandidate

      (maxLatCandidate, minLatCandidate, finalMinLng, finalMaxLng)
    } else {
      // a pole is within the distance
      val finalMinLat = max(minLatCandidate, MIN_LAT)
      val finalMaxLat = min(maxLatCandidate, MAX_LAT)
      val finalMinLon = MIN_LON
      val finalMaxLon = MAX_LON
      (finalMaxLat, finalMinLat, finalMinLon, finalMaxLon)
    }
    BoundedGeoLocation(Coordinate(minLat.toDegrees, minLng.toDegrees), Coordinate(maxLat.toDegrees, maxLng.toDegrees))
  }

}
