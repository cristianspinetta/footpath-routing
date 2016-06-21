package pathgenerator.graph

import org.scalatest.{ FlatSpec, Matchers }

class CoordinateSpec extends FlatSpec with Matchers {

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
}
