package mapdomain.graph

import org.scalatest.{FlatSpec, Matchers}
import utils.DoubleUtils

class CoordinateSpec extends FlatSpec with Matchers {

  val precision = DoubleUtils.DefaultValues.defaultPrecision.value

  "A Coordinate" should "calculate the angle to a given point" in {
    val baseCoordinate: Coordinate = Coordinate(0, 0)
    baseCoordinate.angleTo(baseCoordinate).toDegrees shouldBe 0.0
    baseCoordinate.angleTo(Coordinate(-3, 0)).toDegrees shouldBe 270.0
    baseCoordinate.angleTo(Coordinate(-3, -3)).toDegrees shouldBe 225.0
    baseCoordinate.angleTo(Coordinate(3, -3)).toDegrees shouldBe 135.0
    baseCoordinate.angleTo(Coordinate(0, 3)).toDegrees shouldBe 0.0
    baseCoordinate.angleTo(Coordinate(0, -3)).toDegrees shouldBe 180.0
    baseCoordinate.angleTo(Coordinate(3, 3)).toDegrees shouldBe 45.0
    baseCoordinate.angleTo(Coordinate(3, 0)).toDegrees shouldBe 90.0
  }

  it should "calculate the distance correctly" in {
    val baseCoordinate: Coordinate = Coordinate(-34.615478, -58.399380)
    val destinationCoordinate: Coordinate = Coordinate(-34.574790, -58.493783)

    baseCoordinate.distanceTo(destinationCoordinate) should equal(9.753846675336623 +- precision)
  }
}
