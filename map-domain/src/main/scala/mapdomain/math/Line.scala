package mapdomain.math

import scala.math._

trait Line {
  val slope: Double
  val intercept: Double
  def evaluateByX(x: Double): Double
  def isDefined(point: Point): Boolean
  def createPerpendicular(point: Point): Option[Line]
  def createPerpendicularByX(x: Double): Option[Line]

  /**
   *
   * @param distance: distance between the given line and the new one.
   * @param antiHourDirection: reference to decide where to position the parallel line.
   * @return the new parallel line.
   */
  def createParallel(distance: Double, antiHourDirection: Boolean): Line
}

case class NormalLine(slope: Double, intercept: Double) extends Line {
  import utils.DoubleUtils._

  def isConstant: Boolean = slope ~= 0

  def evaluateByX(x: Double): Double = slope * x + intercept

  def createPerpendicular(point: Point): Option[Line] = {
    if (isDefined(point)) createPerpendicularByX(point.x)
    else None
  }

  override def isDefined(point: Point): Boolean = evaluateByX(point.x) == point.y

  override def createPerpendicularByX(x: Double): Option[Line] = {
    if (isConstant) {
      Some(VerticalLine(x))
    } else {
      val slope2 = -pow(slope, -1)
      val intercept2 = evaluateByX(x) - slope2 * x
      Some(NormalLine(slope2, intercept2))
    }
  }

  override def createParallel(distance: Double, antiHourDirection: Boolean): Line = {
    if (antiHourDirection) NormalLine(slope, intercept + distance)
    else NormalLine(slope, intercept - distance)
  }
}

case class VerticalLine(x: Double) extends Line {
  import utils.DoubleUtils._

  val intercept: Double = Double.NaN
  val slope: Double = Double.PositiveInfinity

  override def isDefined(point: Point): Boolean = x ~= point.x

  def evaluateByX(x2: Double): Double = {
    if (x ~= x2) Double.PositiveInfinity
    else Double.NaN
  }

  def createPerpendicular(point: Point): Option[Line] = {
    if (x ~= point.x) Some(NormalLine(0, point.y))
    else None
  }
  override def createPerpendicularByX(x: Double): Option[Line] = None

  override def createParallel(distance: Double, antiHourDirection: Boolean): Line = {
    if (antiHourDirection) VerticalLine(x - distance)
    else VerticalLine(x + distance)
  }
}

object Line {
  import utils.DoubleUtils._

  def ByPairPoints(p1: Point, p2: Point): Line = {
    if (p1.x ~= p2.x) VerticalLine(p1.x)
    else {
      val slope = (p1.y - p2.y) / (p1.x - p2.x)
      val intercept = p1.y - slope * p1.x
      new NormalLine(slope, intercept)
    }
  }

  def hasIntersection(line1: Line, line2: Line,
    pointEvaluating: Double): Boolean = {
    line1.evaluateByX(pointEvaluating) ~= line2.evaluateByX(pointEvaluating)
  }

  def getIntersectionPoint(l1: Line, l2: Line): Option[Point] = (l1, l2) match {
    case (NormalLine(s1, _), NormalLine(s2, _)) if s1 ~= s2 ⇒ None // they're parallel
    case (VerticalLine(s1), VerticalLine(s2)) if s1 ~= s2   ⇒ Some(Point(s1, Double.PositiveInfinity))
    case (VerticalLine(s1), VerticalLine(s2))               ⇒ None
    case (NormalLine(s1, i1), NormalLine(s2, i2)) ⇒
      val m1 = l1.slope
      val m2 = l2.slope
      val c1 = l1.intercept
      val c2 = l2.intercept

      val x = (c2 - c1) / (m1 - m2)
      val y = l1.evaluateByX(x)

      Some(Point(x, y))
    case (normal: NormalLine, vertical: VerticalLine) ⇒ Some(intersectionPoint(normal, vertical))
    case (vertical: VerticalLine, normal: NormalLine) ⇒ Some(intersectionPoint(normal, vertical))
  }

  protected def intersectionPoint(line: NormalLine, vertical: VerticalLine): Point = {
    Point(vertical.x, line.evaluateByX(vertical.x))
  }

  def areParallel(l1: Line, l2: Line): Boolean = (l1, l2) match {
    case (VerticalLine(_), VerticalLine(_)) ⇒ true
    case (NormalLine(slope1, _), NormalLine(slope2, _)) if slope1 ~= slope2 ⇒ true
    case _ ⇒ false
  }

  def compareParallelsByAltitude(l1: Line, l2: Line): Int = {
    assume(areParallel(l1, l2), s"the provided lines must be parallel")

    (l1, l2) match {
      // 1 -> l1 is to the right of l2, -1 -> l1 is to the left of l2
      case (VerticalLine(x1), VerticalLine(x2))                   ⇒ x1.compare(x2)

      // 1 -> l1 is above of l2, -1 -> l1 is below of l2
      case (NormalLine(_, intercept1), NormalLine(_, intercept2)) ⇒ intercept1.compare(intercept2)
    }
  }
}
