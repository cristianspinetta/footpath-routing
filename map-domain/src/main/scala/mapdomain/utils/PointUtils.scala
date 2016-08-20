package mapdomain.utils

import mapdomain.math.Point

trait PointUtils {

  import utils.DoubleUtils._

  implicit class PointComparator(point: Point) {
    def ~=(other: Point)(implicit precision: Precision = DefaultValues.defaultPrecision): Boolean = {
      (point.x ~= other.x) && (point.y ~= other.y)
    }
  }
}

object PointUtils extends PointUtils
