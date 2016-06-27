package mapgenerator.source

import pathgenerator.graph.Coordinate

case class Bundle(topLeft: Coordinate, topRight: Coordinate,
  bottomRight: Coordinate, bottomLeft: Coordinate)

case class Grid(id: Long, pathTPs: List[Long], bundle: Bundle)

object Grid {

  def create(rows: Int, columns: Int, window: Int)(gridNro: Int, pathTPs: List[Long]): Grid =
    new Grid(gridNro, pathTPs, getBoundForGrid(rows, columns, window, gridNro))

  def getBoundForGrid(rows: Int, columns: Int, window: Int, gridNro: Int): Bundle = {
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
      val beforeBound = getBoundForGrid(rows, columns, window, gridNro - windowsByRow)
      val tl = beforeBound.bottomLeft
      val tr = beforeBound.bottomRight
      val bl = Coordinate(tl.latitude + window, tl.longitude)
      val br = Coordinate(bl.latitude, bl.longitude + window)
      Bundle(tl, tr, br, bl)
    }

  }
}
