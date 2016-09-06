package mapdomain.math

import scala.math._
import utils.DoubleUtils._

case class GVector(source: Point, extreme: Point) {

  val normalizedExtreme = normalizedPoint(extreme)
  val magnitude: Double = sqrt(pow(normalizedExtreme.x, 2) + pow(normalizedExtreme.y, 2))
  val angle: Double = atan2(normalizedExtreme.y, normalizedExtreme.x)
  val θ: Double = angle
  val positiveAngle: Double = if (θ >= 0) θ else 2 * Pi + θ
  val quadrant: Quadrant = defineQuadrant

  lazy val line = Line.ByPairPoints(source, extreme)

  def isDefined(point: Point): Boolean = {
    line.isDefined(point) &&
      (((point.x ~>= source.x) && (point.x ~<= extreme.x)) || ((point.x ~<= source.x) && (point.x ~>= extreme.x))) &&
      (((point.y ~>= source.y) && (point.y ~<= extreme.y)) || ((point.y ~<= source.y) && (point.y ~>= extreme.y)))
  }

  def normalizedPoint(point: Point): Point = Point(point.x - source.x, point.y - source.y)

  def createParallel(distanceForX: Double, distanceForY: Double): GVector =
    GVector(Point(source.x + distanceForX, source.y + distanceForY), Point(extreme.x + distanceForX, extreme.y + distanceForY))

  def invert: GVector = GVector(source = extreme, extreme = source)

  private def defineQuadrant: Quadrant with Product with Serializable = {
    angle match {
      case x if x > (Pi / 2)  ⇒ Quad2
      case x if x >= 0        ⇒ Quad1
      case x if x < (-Pi / 2) ⇒ Quad3
      case _                  ⇒ Quad4
    }
  }
}

object VectorUtils {

  import mapdomain.utils.PointUtils._

  /**
   * Create a parallel vector keeping the direction
   * @param vector: the reference Vector
   * @param distance: The distance between the vector and its parallel
   * @param antiHourRotation: Define the side where the parallel will be created
   * @return The parallel Vector
   */
  def createParallelVector(vector: GVector, distance: Double, antiHourRotation: Boolean): GVector = (vector.quadrant, antiHourRotation) match {
    case (Quad1, true) ⇒
      val α = (Pi / 2) - vector.θ
      val (dX, dY) = (-cos(α) * distance, sin(α) * distance)
      vector.createParallel(dX, dY)
    case (Quad1, false) ⇒
      val α = (Pi / 2) - vector.θ
      val (dX, dY) = (cos(α) * distance, -sin(α) * distance)
      vector.createParallel(dX, dY)
    case (Quad2, true) ⇒
      val α = Pi - vector.θ
      val (dX, dY) = (-sin(α) * distance, -cos(α) * distance)
      vector.createParallel(dX, dY)
    case (Quad2, false) ⇒
      val α = Pi - vector.θ
      val (dX, dY) = (sin(α) * distance, cos(α) * distance)
      vector.createParallel(dX, dY)
    case (Quad3, true) ⇒
      val α = Pi - abs(vector.θ)
      val (dX, dY) = (sin(α) * distance, -cos(α) * distance)
      vector.createParallel(dX, dY)
    case (Quad3, false) ⇒
      val α = Pi - abs(vector.θ)
      val (dX, dY) = (-sin(α) * distance, cos(α) * distance)
      vector.createParallel(dX, dY)
    case (Quad4, true) ⇒
      val α = abs(vector.θ)
      val (dX, dY) = (sin(α) * distance, cos(α) * distance)
      vector.createParallel(dX, dY)
    case (Quad4, false) ⇒
      val α = abs(vector.θ)
      val (dX, dY) = (-sin(α) * distance, -cos(α) * distance)
      vector.createParallel(dX, dY)
  }

  def getIntersectionPoint(vector1: GVector, vector2: GVector): Option[Point] = (vector1, vector2) match {
    case (GVector(source1, extreme1), GVector(source2, extreme2)) if (source1 ~= source2) || (source1 ~= extreme2)   ⇒ Some(source1)
    case (GVector(source1, extreme1), GVector(source2, extreme2)) if (extreme1 ~= source2) || (extreme1 ~= extreme2) ⇒ Some(extreme1)
    case (v1, v2) ⇒
      val line1 = Line.ByPairPoints(v1.source, v1.extreme)
      val line2 = Line.ByPairPoints(v2.source, v2.extreme)
      Line.getIntersectionPoint(line1, line2)
  }

  def angleBetweenInAntiHourRotation(vector1: GVector, vector2: GVector): Double = {
    if (vector2.positiveAngle > vector1.positiveAngle)
      vector2.positiveAngle - vector1.positiveAngle
    else if (vector1.positiveAngle > vector2.positiveAngle)
      (2 * Pi) - vector1.positiveAngle + vector2.positiveAngle
    else
      0
  }
}

trait Quadrant
case object Quad1 extends Quadrant
case object Quad2 extends Quadrant
case object Quad3 extends Quadrant
case object Quad4 extends Quadrant
