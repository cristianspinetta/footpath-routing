package mapdomain.math

import org.scalatest._
import utils.DoubleUtils

class LineSpec extends FlatSpec with Matchers {

  val precision = DoubleUtils.DefaultValues.defaultPrecision.value

  "A Line Object" should "create a perpendicular line from the given one" in {

    val line = NormalLine(1, 0)
    val perpendicular = line.createPerpendicularByX(2).get
    val perpendicularPerpendicular = perpendicular.createPerpendicularByX(2).get

    perpendicular.slope should equal(-1.0 +- precision)
    perpendicular.intercept should equal(4.0 +- precision)
    line.slope should equal(perpendicularPerpendicular.slope +- precision)
    line.intercept should equal(perpendicularPerpendicular.intercept +- precision)
  }

  it should "create a parallel line for a Normal Line correctly" in {
    val line = NormalLine(1, 0)
    val parallelUp = line.createParallel(1, antiHourDirection = true)
    val parallelDown = line.createParallel(1, antiHourDirection = false)

    parallelUp.slope should equal(line.slope +- precision)
    parallelUp.intercept should equal(line.intercept + 1 +- precision)
    parallelDown.slope should equal(line.slope +- precision)
    parallelDown.intercept should equal(line.intercept - 1 +- precision)

    val parallelOfParallel = parallelUp.createParallel(1, antiHourDirection = false)

    // It should be equal to the first line
    parallelOfParallel.slope should equal(line.slope +- precision)
    parallelOfParallel.intercept should equal(line.intercept +- precision)
  }

  it should "compare two parallel lines by anti hour rotation correctly" in {
    val line = NormalLine(1, 0)
    val parallelUp = line.createParallel(1, antiHourDirection = true)
    val parallelDown = line.createParallel(1, antiHourDirection = false)

    val rotation: Int = Line.compareParallelsByAltitude(parallelUp, parallelDown)

    rotation should be(1)
  }
}
