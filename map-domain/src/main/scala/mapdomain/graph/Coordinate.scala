package mapdomain.graph

import mapdomain.math.Point

import scala.math._
import scalikejdbc._

/**
 *
 * @param latitude: in degrees
 * @param longitude: in degrees
 * @param id: optional
 */
case class Coordinate(latitude: Double, longitude: Double, override val id: Option[Long] = None) extends BaseEntity {
  private val φ1: Double = latitude.toRadians

  val radLatitude: Double = latitude.toRadians
  val radLongitude: Double = longitude.toRadians

  /**
   * The distance between this point and the given one in km.
   *
   * @param to: point to where the distance is calculated.
   * @return the distance in meters.
   */
  def distanceTo(to: Coordinate): Double = {
    // see http://www.movable-type.co.uk/scripts/latlong.html
    val φ2 = toRadians(to.latitude)
    val Δφ = toRadians(to.latitude - latitude)
    val Δλ = toRadians(to.longitude - longitude)
    val a = pow(sin(Δφ / 2), 2) + cos(φ1) * cos(φ2) * pow(sin(Δλ / 2), 2)
    2 * atan2(sqrt(a), sqrt(1 - a)) * Coordinate.radius
  }

  /**
   * The distance between this point and the given one in degrees.
   *
   * @param to: point to where the distance is calculated.
   * @return the distance in meters.
   */
  def distanceToInDegrees(to: Coordinate): Double = {
    // @see http://www.movable-type.co.uk/scripts/latlong.html
    val φ2 = toRadians(to.latitude)
    val Δφ = toRadians(to.latitude - latitude)
    val Δλ = toRadians(to.longitude - longitude)
    val a = pow(sin(Δφ / 2), 2) + cos(φ1) * cos(φ2) * pow(sin(Δλ / 2), 2)
    toDegrees(2 * atan2(sqrt(a), sqrt(1 - a)))
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

  def toPoint: Point = Point(longitude, latitude)

  override def toString: String = s"Coordinate(Lat: $latitude, Lng: $longitude)"
}

object Coordinate extends SQLSyntaxSupport[Coordinate] {
  val radius: Double = 6371 // Km

  override val tableName = "Coordinate"

  def fromPoint(point: Point): Coordinate = Coordinate(point.y, point.x)

  /**
   *
   * @param source: source in decimal degree
   * @param bearing: the bearing from the start point (clockwise from north)
   * @param distance: the distance travelled in meters
   * @return
   */
  def destinationPointByDistanceAndBearing(source: Coordinate, bearing: Double, distance: Double): Coordinate = {
    val δ = distance / radius
    val θ = toRadians(bearing)
    val (φ1, λ1) = (toRadians(source.latitude), toRadians(source.longitude))
    val φ2 = asin(sin(φ1) * cos(δ) + cos(φ1) * sin(δ) * cos(θ))
    val λ2 = λ1 + atan2(sin(θ) * sin(δ) * cos(φ1), cos(δ) - sin(φ1) * sin(φ2))
    Coordinate(φ2, λ2)
  }
}

trait CoordinateRepository {

  val c = Coordinate.syntax("c")

  def coordinate(c: SyntaxProvider[Coordinate])(rs: WrappedResultSet): Coordinate = coordinate(c.resultName)(rs)

  private def coordinate(c: ResultName[Coordinate])(implicit rs: WrappedResultSet): Coordinate = {
    new Coordinate(latitude = rs.double(c.latitude), longitude = rs.double(c.longitude), id = Some(rs.long(c.id)))
  }

  def create(latitude: Double, longitude: Double)(implicit session: DBSession = Coordinate.autoSession): Coordinate = {
    val id = withSQL {
      insert.into(Coordinate).namedValues(
        Coordinate.column.latitude -> latitude,
        Coordinate.column.longitude -> longitude)
    }.updateAndReturnGeneratedKey.apply()

    Coordinate(latitude, longitude, Some(id))
  }

  def find(id: Long)(implicit session: DBSession = Coordinate.autoSession): Option[Coordinate] = {
    withSQL { select.from(Coordinate as c).where.eq(c.id, id) }
      .map(coordinate(c)).single.apply()
  }

  def save(coord: Coordinate)(implicit session: DBSession = Coordinate.autoSession): Coordinate = {
    withSQL {
      update(Coordinate).set(
        Coordinate.column.latitude -> coord.latitude,
        Coordinate.column.longitude -> coord.longitude).where.eq(Coordinate.column.id, coord.id)
    }.update.apply()
    coord
  }

  def delete(coord: Coordinate)(implicit session: DBSession = Coordinate.autoSession): Unit = withSQL {
    deleteFrom(Coordinate).where.eq(Coordinate.column.id, coord.id)
  }.update.apply()

  def deleteAll(implicit session: DBSession = Coordinate.autoSession): Unit = withSQL {
    deleteFrom(Coordinate)
  }.update.apply()

}

object CoordinateRepository extends CoordinateRepository
