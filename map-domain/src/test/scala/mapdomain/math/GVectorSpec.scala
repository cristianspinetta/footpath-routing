package mapdomain.math

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.math._

class GVectorSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

  val precision: Double = pow(10, -8)
  val distance: Double = 1

  val sampleLimit = 5000

  "A GVector Object" when {

    val source = Point(0, 0)

    "it's over the first quadrant" should {

      val validNumbers = for (n ← Gen.choose(0, sampleLimit)) yield n

      "create parallel correctly for positive anti hour rotation" in {

        forAll(validNumbers, validNumbers) { (x: Int, y: Int) ⇒

          whenever(x >= 0 && y >= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = true)

            val intercept = distance / cos(vector.θ)

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(-1)
          }
        }
      }

      "create parallel correctly for negative anti hour rotation" in {

        forAll(validNumbers, validNumbers) { (x: Int, y: Int) ⇒

          whenever(x >= 0 && y >= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = false)

            val intercept = -(distance / cos(vector.θ))

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(1)
          }
        }
      }
    }

    "it's over the second quadrant" should {

      val validNumbersForX = for (n ← Gen.choose(-sampleLimit, 0)) yield n
      val validNumbersForY = for (n ← Gen.choose(0, sampleLimit)) yield n

      "create parallel correctly for positive anti hour rotation" in {

        forAll(validNumbersForX, validNumbersForY) { (x: Int, y: Int) ⇒

          whenever(x <= 0 && y >= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = true)

            val α = Pi - vector.θ

            val intercept = -(distance / cos(α))

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(1)
          }
        }
      }

      "create parallel correctly for negative anti hour rotation" in {

        forAll(validNumbersForX, validNumbersForY) { (x: Int, y: Int) ⇒

          whenever(x <= 0 && y >= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = false)

            val α = Pi - vector.θ

            val intercept = distance / cos(α)

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(-1)
          }
        }
      }
    }

    "it's over the third quadrant" should {

      val validNumbersForX = for (n ← Gen.choose(-sampleLimit, 0)) yield n
      val validNumbersForY = for (n ← Gen.choose(-sampleLimit, 0)) yield n

      "create parallel correctly for positive anti hour rotation" in {

        forAll(validNumbersForX, validNumbersForY) { (x: Int, y: Int) ⇒

          whenever(x <= 0 && y <= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = true)

            val α = Pi + vector.θ

            val intercept = -(distance / cos(α))

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(1)
          }
        }
      }

      "create parallel correctly for negative anti hour rotation" in {

        forAll(validNumbersForX, validNumbersForY) { (x: Int, y: Int) ⇒

          whenever(x <= 0 && y <= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = false)

            val α = Pi + vector.θ

            val intercept = distance / cos(α)

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(-1)
          }
        }
      }
    }

    "it's over the fourth quadrant" should {

      val validNumbersForX = for (n ← Gen.choose(0, sampleLimit)) yield n
      val validNumbersForY = for (n ← Gen.choose(-sampleLimit, 0)) yield n

      "create parallel correctly for positive anti hour rotation" in {

        forAll(validNumbersForX, validNumbersForY) { (x: Int, y: Int) ⇒

          whenever(x >= 0 && y <= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = true)

            val α = abs(vector.θ)

            val intercept = distance / cos(α)

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(-1)
          }
        }
      }

      "create parallel correctly for negative anti hour rotation" in {

        forAll(validNumbersForX, validNumbersForY) { (x: Int, y: Int) ⇒

          whenever(x >= 0 && y <= 0 && !(x == 0 && y == 0)) {
            val vector = GVector(source, Point(x, y))

            val parallel = VectorUtils.createParallelVector(vector, distance, antiHourRotation = false)

            val α = abs(vector.θ)

            val intercept = -(distance / cos(α))

            val lineForVector = Line.ByPairPoints(vector.source, vector.extreme)
            val lineForParallel = Line.ByPairPoints(parallel.source, parallel.extreme)

            parallel.angle should equal(vector.angle +- precision)
            lineForParallel.slope should equal(lineForVector.slope +- precision)
            lineForParallel.intercept should equal(intercept +- precision)
            Line.areParallel(lineForVector, lineForParallel) should be(true)
            Line.compareParallelsByAltitude(lineForVector, lineForParallel) should equal(1)
          }
        }
      }
    }
  }
}
