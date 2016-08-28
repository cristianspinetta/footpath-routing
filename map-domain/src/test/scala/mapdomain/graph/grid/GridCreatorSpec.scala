package mapdomain.graph.grid

import mapdomain.graph.Coordinate
import mapdomain.publictransport.PathTP
import org.scalatest.{ FlatSpec, Matchers }

class GridCreatorSpec extends FlatSpec with Matchers {

  val pathTPs: List[PathTP] = PathTP(List(
    List[(Double, Double)]((1, 2), (2, 2), (3, 2), (4, 2), (5, 2),
      (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7), (6, 8), (6, 9), (6, 10)),
    List[(Double, Double)]((1, 3), (2, 3), (3, 3), (4, 3), (5, 3),
      (6, 3), (7, 3), (7, 4), (7, 5), (7, 6), (7, 7), (7, 8), (7, 9), (7, 10)),
    List[(Double, Double)]((10, 5), (9, 5), (8, 5), (7, 5), (6, 5), (5, 5),
      (4, 5), (3, 5), (3, 4), (3, 3), (3, 2), (3, 1)),
    List[(Double, Double)]((1, 8), (2, 8), (3, 8), (4, 8), (5, 8), (6, 8),
      (7, 8), (8, 8), (8, 9), (8, 10))))

  "The Cell Creator" should "create the grids correctly" in {

    val combinationsGrid: List[Cell] = List(
      Cell(1, List(1, 2, 3),
        Bundle(Coordinate(1, 1), Coordinate(1, 3), Coordinate(3, 3), Coordinate(3, 1))),
      Cell(2, List(2, 3),
        Bundle(Coordinate(1, 3), Coordinate(1, 5), Coordinate(3, 5), Coordinate(3, 3))),
      Cell(3, List(3),
        Bundle(Coordinate(1, 5), Coordinate(1, 7), Coordinate(3, 7), Coordinate(3, 5))),
      Cell(4, List(4),
        Bundle(Coordinate(1, 7), Coordinate(1, 9), Coordinate(3, 9), Coordinate(3, 7))),
      Cell(5, List(),
        Bundle(Coordinate(1, 9), Coordinate(1, 11), Coordinate(3, 11), Coordinate(3, 9))),
      Cell(6, List(1, 2),
        Bundle(Coordinate(3, 1), Coordinate(3, 3), Coordinate(5, 3), Coordinate(5, 1))),
      Cell(7, List(2, 3),
        Bundle(Coordinate(3, 3), Coordinate(3, 5), Coordinate(5, 5), Coordinate(5, 3))),
      Cell(8, List(3),
        Bundle(Coordinate(3, 5), Coordinate(3, 7), Coordinate(5, 7), Coordinate(5, 5))),
      Cell(9, List(4),
        Bundle(Coordinate(3, 7), Coordinate(3, 9), Coordinate(5, 9), Coordinate(5, 7))),
      Cell(10, List(4),
        Bundle(Coordinate(3, 9), Coordinate(3, 11), Coordinate(5, 11), Coordinate(5, 9))))

    combinationsGrid.foreach(grid ⇒ println(s"$grid"))

    val creator = Cell.create(12, 2) _

    val combinationsGridMoreFun: List[Cell] = List(
      creator(1, List(1, 2, 3)),
      creator(2, List(2, 3)),
      creator(3, List(3)),
      creator(4, List(4)),
      creator(5, List()),
      creator(6, List(1, 2)),
      creator(7, List(2, 3)),
      creator(8, List(3)),
      creator(9, List(4)),
      creator(10, List(4)))

    combinationsGridMoreFun.foreach(grid ⇒ println(s"$grid"))

    combinationsGrid should be(combinationsGridMoreFun)
  }
}
