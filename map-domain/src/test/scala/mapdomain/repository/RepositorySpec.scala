package mapdomain.repository

import mapdomain.graph.{ Coordinate, CoordinateRepository }
import mapdomain.publictransport.PathRepository
import mapdomain.sidewalk.{ Ramp, RampRepository }
import mapdomain.street.{ OsmStreetEdge, OsmStreetEdgeRepository, OsmVertexRepository }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
import scalikejdbc.config.DBs

class RepositorySpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  override def beforeAll() {
    DBs.setupAll()
    new DBInitializer().start()
  }

  override def afterEach(): Unit = {
    OsmStreetEdgeRepository.deleteAll
    OsmVertexRepository.deleteAll
    RampRepository.deleteAll
    CoordinateRepository.deleteAll
    PathRepository.deleteAll
  }

  "With database configurated" should "create coordinates correctly" in {
    var coordinate: Coordinate = CoordinateRepository.create(10, 20)
    coordinate.id should not be None
    coordinate = CoordinateRepository.find(coordinate.id.get).get
    coordinate.latitude shouldBe 10
    coordinate.longitude shouldBe 20
  }

  it should "create ramps correctly" in {
    var ramp: Ramp = RampRepository.create(11, 12, "16", "Callao", Some(500), "Callao 523")
    ramp.id shouldBe "16"
    ramp = RampRepository.find(ramp.id).get
    ramp.coordinate.latitude shouldBe 11
    ramp.coordinate.longitude shouldBe 12
    ramp.street shouldBe "Callao"
    ramp.number shouldBe Some(500)
    ramp.address shouldBe "Callao 523"
  }

  it should "create edges correctly" in {
    val vertexStart = OsmVertexRepository.create(5, 12, 11)
    val vertexEnd = OsmVertexRepository.create(6, 14, 13)
    val edgeId = OsmStreetEdgeRepository.create(vertexStart.id, vertexEnd.id, 10d, 9l)

    val edge: OsmStreetEdge = OsmStreetEdgeRepository.find(edgeId)
    edge.id should not be None
    edge.distance shouldBe 10d
    edge.wayId shouldBe 9l
    edge.vertexStart shouldBe 5
    edge.vertexEnd shouldBe 6
    edge.osmVertexStart.id shouldBe 5
    edge.osmVertexStart.coordinate.latitude shouldBe 12
    edge.osmVertexStart.coordinate.longitude shouldBe 11
    edge.osmVertexEnd.coordinate.latitude shouldBe 14
    edge.osmVertexEnd.coordinate.longitude shouldBe 13
  }

  it should "create vertex correctly" in {
    var vertex1 = OsmVertexRepository.create(5, 12, 11)
    vertex1 = OsmVertexRepository.find(vertex1.id).get
    vertex1.id shouldBe 5
    vertex1.coordinate.latitude shouldBe 12
    vertex1.coordinate.longitude shouldBe 11

    val vertex2 = OsmVertexRepository.create(6, 13, 14)
    val vertex3 = OsmVertexRepository.create(7, 14, 15)

    val firstEdgeId = OsmStreetEdgeRepository.create(vertex1.id, vertex2.id, 10d, 9l)
    val secondEdgeId = OsmStreetEdgeRepository.create(vertex1.id, vertex3.id, 10d, 9l)
    val thirdEdgeId = OsmStreetEdgeRepository.create(vertex2.id, vertex1.id, 10d, 9l)

    var edges = OsmVertexRepository.findEdges(vertex1.id)
    edges.size shouldBe 2
    edges.map(e ⇒ e.id.get) should contain only (firstEdgeId, secondEdgeId)

    edges = OsmVertexRepository.findEdges(vertex2.id)
    edges.size shouldBe 1
    edges.head.id.get shouldBe thirdEdgeId
  }

  it should "create paths correctly" in {
    val coordinates = "{lng: 34, lat: 20}, {lng: 34, lat: 21}"
    var path = PathRepository.create(coordinates)
    path.id should not be None

    path = PathRepository.find(path.id.get).get
    path.id should not be None
    path.coordinates shouldBe coordinates
  }

}
