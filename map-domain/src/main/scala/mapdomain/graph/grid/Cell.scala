package mapdomain.graph.grid

import mapdomain.graph.{ Coordinate, CoordinateRepository }

case class Bundle(topLeft: Coordinate, topRight: Coordinate,
    bottomRight: Coordinate, bottomLeft: Coordinate) {

  lazy val width = topRight.longitude - topLeft.longitude
  lazy val hight = topLeft.longitude - bottomLeft.longitude
}

case class GridProperty()

case class Grid(granular: Int, mapBundle: Bundle) {
  val window: Double = mapBundle.width / granular
}

case class Cell(id: Long, pathTPs: List[Long], bundle: Bundle)

object Cell {

  def create(columns: Int, window: Int)(gridNro: Int, pathTPs: List[Long]): Cell =
    new Cell(gridNro, pathTPs, getBoundForCell(columns, window, gridNro))

  def getBoundForCell(columns: Int, window: Int, gridNro: Int): Bundle = {
    val windowsByRow = (columns - 1) / window
    val restForPosByRow = gridNro % windowsByRow
    val posByRow = if (restForPosByRow == 0) windowsByRow else restForPosByRow
    if (gridNro <= windowsByRow) { // First row
      val tl = Coordinate(1, (posByRow - 1) * window + 1)
      val tr = Coordinate(1, tl.longitude + window)
      val bl = Coordinate(1 + window, tl.longitude)
      val br = Coordinate(bl.latitude, bl.longitude + window)
      Bundle(tl, tr, br, bl)
    } else {
      val beforeBound = getBoundForCell(columns, window, gridNro - windowsByRow)
      val tl = beforeBound.bottomLeft
      val tr = beforeBound.bottomRight
      val bl = Coordinate(tl.latitude + window, tl.longitude)
      val br = Coordinate(bl.latitude, bl.longitude + window)
      Bundle(tl, tr, br, bl)
    }

  }
}
