package utils

import scala.math._

trait DoubleUtils {

  import utils.DoubleUtils._

  implicit class DoubleComparator(value: Double) {

    def ~=(other: Double)(implicit precision: Precision = DefaultValues.defaultPrecision): Boolean = {
      if ((value - other).abs < precision.value) true else false
    }

    def ~>=(other: Double)(implicit precision: Precision = DefaultValues.defaultPrecision): Boolean = {
      if (value > other || (value - other).abs < precision.value) true else false
    }

    def ~<=(other: Double)(implicit precision: Precision = DefaultValues.defaultPrecision): Boolean = {
      if (value < other || (value - other).abs < precision.value) true else false
    }
  }

  implicit class DoubleReadable(value: Double) {

    def readable: String = f"$value%1.2f"
  }
}

object DoubleUtils extends DoubleUtils {

  case class Precision(value: Double)

  object DefaultValues {
    implicit val defaultPrecision: Precision = Precision(pow(10, -10))
  }

}

