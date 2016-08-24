package mapdomain

import mapdomain.graph.Coordinate
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}
import scalikejdbc.config.DBs
import mapdomain.graph.CoordinateRepository
import mapdomain.sidewalk.{Ramp, RampRepository}
import mapdomain.street.{OsmStreetEdge, OsmStreetEdgeRepository, OsmVertexRepository}

class RepositorySpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  override def beforeAll() {
    DBs.setupAll()
  }

  override def afterEach(): Unit = {
    OsmStreetEdgeRepository.deleteAll()
    OsmVertexRepository.deleteAll()
    RampRepository.deleteAll()
    CoordinateRepository.deleteAll()
  }

  "With database configurated" should "create coordinates correctly" in {
    var coordinate: Coordinate= CoordinateRepository.create(10, 20)
    coordinate.id should not be None
    coordinate = CoordinateRepository.find(coordinate.id.get).get
    coordinate.latitude shouldBe 10
    coordinate.longitude shouldBe 20
  }

  "With database configurated" should "create ramps correctly" in {
    var ramp: Ramp = RampRepository.create(11, 12, "16", "Callao", Some(500), "Callao 523")
    ramp.id shouldBe "16"
    ramp = RampRepository.find(ramp.id.toLong).get
    ramp.coordinate.latitude shouldBe 11
    ramp.coordinate.longitude shouldBe 12
    ramp.street shouldBe "Callao"
    ramp.number shouldBe Some(500)
    ramp.address shouldBe "Callao 523"
  }

  "With database configurated" should "create edges correctly" in {
    val vertexStart = OsmVertexRepository.create(5, 12, 11)
    val vertexEnd = OsmVertexRepository.create(6, 14, 13)
    var edge: OsmStreetEdge = OsmStreetEdgeRepository.create(vertexStart.id, vertexEnd.id, 10d, 9l)
    edge.id should not be None
    edge = OsmStreetEdgeRepository.find(edge.id.get)
    edge.distance shouldBe 10d
    edge.wayId shouldBe 9l
    edge.vertexStart shouldBe 5
    edge.vertexEnd shouldBe 6
  }

}
