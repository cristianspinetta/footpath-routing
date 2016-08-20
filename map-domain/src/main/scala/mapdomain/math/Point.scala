package mapdomain.math

import scala.math._

case class Point(x: Double, y: Double)

object Point {

  def distance(p1: Point, p2: Point): Double = sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2))

  /**
   * The angle between segment 1-2 and 1-3, applying The Law of Cosines.
   */
  def angleByThreePoints(p1: Point, p2: Point, p3: Point): Double = {
    val s12 = distance(p1, p2)
    val s23 = distance(p2, p3)
    val s13 = distance(p1, p3)
    acos((pow(s12, 2) + pow(s13, 2) - pow(s23, 2)) / (2 * s12 * s13))
  }
}
