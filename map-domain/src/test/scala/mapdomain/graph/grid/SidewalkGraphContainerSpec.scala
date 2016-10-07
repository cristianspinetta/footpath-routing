package mapdomain.graph.grid

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.{ InMemorySidewalkGraphContainer, _ }
import org.scalatest.{ FlatSpec, Ignore, Matchers }

@Ignore class SidewalkGraphContainerSpec extends FlatSpec with Matchers {

  "With in memory sidewalk graph container" should "filter unaccessible edges and ramps" in {
    var sidewalk1 = SidewalkVertex(1, Coordinate(10, 9), Nil, Nil, 1)
    val sidewalk2 = SidewalkVertex(2, Coordinate(10, 9), Nil, Nil, 1)
    val sidewalk3 = SidewalkVertex(3, Coordinate(10, 9), Nil, Nil, 1)
    val sidewalk4 = SidewalkVertex(4, Coordinate(10, 9), Nil, Nil, 1)
    val sidewalk5 = SidewalkVertex(5, Coordinate(10, 9), Nil, Nil, 1)

    val ramp1 = Ramp(Coordinate(10, 9), Some(1), "Callao 1234", true)
    val ramp2 = Ramp(Coordinate(10, 9), Some(2), "Callao 1234", true)
    val ramp3 = Ramp(Coordinate(10, 9), Some(3), "Callao 1234", true)
    val ramp4 = Ramp(Coordinate(10, 9), Some(4), "Callao 1234", true)

    val edge1 = SidewalkEdge(sidewalk2.id, sidewalk1.id, "key1", NorthSide, None)
    val edge2 = SidewalkEdge(sidewalk1.id, sidewalk3.id, "key2", NorthSide, None)
    val crossingEdge3 = StreetCrossingEdge(sidewalk1.id, sidewalk4.id, "key3", None, ramp1.id, ramp2.id)
    val crossingEdge4 = StreetCrossingEdge(sidewalk5.id, sidewalk1.id, "key4", None, ramp3.id, ramp4.id)

    sidewalk1 = sidewalk1.copy(sidewalkEdges = List(edge1, edge2), streetCrossingEdges = List(crossingEdge3, crossingEdge4))

    val sidewalkContainer = InMemorySidewalkGraphContainer(List(sidewalk1, sidewalk2, sidewalk3, sidewalk4, sidewalk5))
    sidewalkContainer.ramps = List(ramp1, ramp2, ramp3, ramp4)

    // all neighbours are accessible
    var neighbours = sidewalkContainer.neighbours(sidewalk1)
    neighbours.size shouldBe 4
    neighbours = neighbours.sortWith(_.id < _.id)
    neighbours.head.id shouldBe sidewalk2.id
    neighbours.tail.head.id shouldBe sidewalk3.id
    neighbours.tail.tail.head.id shouldBe sidewalk4.id
    neighbours.tail.tail.tail.head.id shouldBe sidewalk5.id

    // findNeighbours should filter not accessible edge and ramps
    //    edge1.isAccessible = false
    ramp1.isAccessible = false
    neighbours = sidewalkContainer.neighbours(sidewalk1)
    neighbours.size shouldBe 2
    neighbours = neighbours.sortWith(_.id < _.id)
    neighbours.head.id shouldBe sidewalk3.id
    neighbours.tail.head.id shouldBe sidewalk5.id

    // findNeighbours should filter crossing edges without ramps
    //    crossingEdge4.rampStartId = None
    neighbours = sidewalkContainer.neighbours(sidewalk1)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe sidewalk3.id

    // findNeighbours without neighbours
    //    edge2.isAccessible = false
    neighbours = sidewalkContainer.neighbours(sidewalk1)
    neighbours.size shouldBe 0
  }

}
