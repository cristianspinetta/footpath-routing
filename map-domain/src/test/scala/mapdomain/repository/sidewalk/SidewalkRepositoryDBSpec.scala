package mapdomain.repository.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.repository.BaseRepositoryDBSpec
import mapdomain.repository.street.{ StreetEdgeRepository, StreetInfoRepository, StreetVertexRepository }
import mapdomain.sidewalk._
import mapdomain.street._
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
import scalikejdbc.config.DBs

import scala.math._

class SidewalkRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "Sidewalk Repository" should "create sidewalks correctly" in {
    val vertex1 = StreetVertexRepository.create(StreetVertex(1, Nil, Coordinate(10, 9)))
    var sidewalk1 = SidewalkVertexRepository.create(SidewalkVertex(1, Coordinate(10, 9), Nil, Nil, vertex1.id))
    sidewalk1 = SidewalkVertexRepository.find(sidewalk1.id).get
    sidewalk1.id shouldBe 1
    coordinateAssertion(sidewalk1.coordinate, Coordinate(10, 9))
    sidewalk1.streetVertexBelongToId shouldBe 1

    val sidewalk2 = SidewalkVertexRepository.create(SidewalkVertex(2, Coordinate(10, 9), Nil, Nil, vertex1.id))
    val sidewalk3 = SidewalkVertexRepository.create(SidewalkVertex(3, Coordinate(10, 9), Nil, Nil, vertex1.id))
    val sidewalk4 = SidewalkVertexRepository.create(SidewalkVertex(4, Coordinate(10, 9), Nil, Nil, vertex1.id))
    val sidewalk5 = SidewalkVertexRepository.create(SidewalkVertex(5, Coordinate(10, 9), Nil, Nil, vertex1.id))

    val ramp1 = RampRepository.create(10, 9, "Callao 1234", isAccessible = true)
    val ramp2 = RampRepository.create(10, 9, "Callao 1234", isAccessible = true)
    val ramp3 = RampRepository.create(10, 9, "Callao 1234", isAccessible = true)
    val ramp4 = RampRepository.create(10, 9, "Callao 1234", isAccessible = true)

    val wayId: Long = 2
    val streetInfoId = StreetInfoRepository.create(StreetInfo(None, Some("an address"), wayId))

    val streetEdge = StreetEdge(5, 6, 10, wayId, streetInfoId, None)
    val edgeId = StreetEdgeRepository.create(streetEdge)
    val savedStreetEdge: StreetEdge = StreetEdgeRepository.find(edgeId)
    val edge1Id = SidewalkEdgeRepository.create(SidewalkEdge(sidewalk2.id, sidewalk1.id, "key1", NorthSide, savedStreetEdge.id))
    val edge2Id = SidewalkEdgeRepository.create(SidewalkEdge(sidewalk1.id, sidewalk3.id, "key2", NorthSide, savedStreetEdge.id))
    StreetCrossingEdgeRepository.create(StreetCrossingEdge(sidewalk1.id, sidewalk4.id, "key3", None, ramp1.id, ramp2.id))
    val crossingEdge2Id = StreetCrossingEdgeRepository.create(StreetCrossingEdge(sidewalk5.id, sidewalk1.id, "key4", None, ramp3.id, ramp4.id))

    // all neighbours are accessible
    var neighbours = SidewalkVertexRepository.findNeighbours(sidewalk1.id)
    neighbours.size shouldBe 4
    neighbours = neighbours.sortWith(_.id < _.id)
    neighbours.head.id shouldBe sidewalk2.id
    neighbours.tail.head.id shouldBe sidewalk3.id
    neighbours.tail.tail.head.id shouldBe sidewalk4.id
    neighbours.tail.tail.tail.head.id shouldBe sidewalk5.id

    neighbours = SidewalkVertexRepository.findNeighbours(sidewalk2.id)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe sidewalk1.id

    neighbours = SidewalkVertexRepository.findNeighbours(sidewalk3.id)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe sidewalk1.id

    neighbours = SidewalkVertexRepository.findNeighbours(sidewalk4.id)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe sidewalk1.id

    neighbours = SidewalkVertexRepository.findNeighbours(sidewalk5.id)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe sidewalk1.id

    // findNeighbours should filter not accessible edge and ramps
    val edge1 = SidewalkEdgeRepository.find(edge1Id)
    SidewalkEdgeRepository.save(edge1.copy(isAccessible = false))
    ramp1.isAccessible = false
    RampRepository.save(ramp1)

    neighbours = SidewalkVertexRepository.findNeighbours(sidewalk1.id)
    neighbours.size shouldBe 2
    neighbours = neighbours.sortWith(_.id < _.id)
    neighbours.head.id shouldBe sidewalk3.id
    neighbours.tail.head.id shouldBe sidewalk5.id

    // findNeighbours should filter crossing edges without ramps
    StreetCrossingEdgeRepository.deleteStartRamp(crossingEdge2Id)
    neighbours = SidewalkVertexRepository.findNeighbours(sidewalk1.id)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe sidewalk3.id

    // findNeighbours without neighbours
    val edge2 = SidewalkEdgeRepository.find(edge2Id)
    SidewalkEdgeRepository.save(edge2.copy(isAccessible = false))
    neighbours = SidewalkVertexRepository.findNeighbours(sidewalk1.id)
    neighbours.size shouldBe 0
  }

  it should "create sidewalk crossing edges correctly" in {
    val vertex1 = StreetVertexRepository.create(StreetVertex(5, Nil, Coordinate(12, 11)))
    val sidewalk1 = SidewalkVertexRepository.create(SidewalkVertex(4, Coordinate(10, 9), Nil, Nil, vertex1.id))
    val sidewalk2 = SidewalkVertexRepository.create(SidewalkVertex(5, Coordinate(11, 19), Nil, Nil, vertex1.id))

    val streetCrossingEdge1Id = StreetCrossingEdgeRepository.create(StreetCrossingEdge(sidewalk1.id, sidewalk2.id, "key1"))
    val streetCrossingEdge1 = StreetCrossingEdgeRepository.find(streetCrossingEdge1Id)
    streetCrossingEdge1.id should not be None
    streetCrossingEdge1.keyValue shouldBe "key1"
    streetCrossingEdge1.vertexStartId shouldBe sidewalk1.id
    streetCrossingEdge1.vertexEndId shouldBe sidewalk2.id

    val crossingEdges = StreetCrossingEdgeRepository.findCrossingEdgesBySidewalkVertex(sidewalk1.id)
    crossingEdges.size shouldBe 1
    crossingEdges.head.id.get shouldBe streetCrossingEdge1Id
    crossingEdges.head.keyValue shouldBe "key1"
  }

  it should "create sidewalk edges correctly" in {
    val vertex1 = StreetVertexRepository.create(StreetVertex(5, Nil, Coordinate(12, 11)))
    StreetVertexRepository.create(StreetVertex(6, Nil, Coordinate(14, 13)))
    val sidewalk1 = SidewalkVertexRepository.create(SidewalkVertex(4, Coordinate(10, 9), Nil, Nil, vertex1.id))
    val sidewalk2 = SidewalkVertexRepository.create(SidewalkVertex(5, Coordinate(11, 19), Nil, Nil, vertex1.id))

    val wayId: Long = 2
    val streetInfoId = StreetInfoRepository.create(StreetInfo(None, Some("an address"), wayId))

    val streetEdge = StreetEdge(5, 6, 10, wayId, streetInfoId, None)
    val edgeId = StreetEdgeRepository.create(streetEdge)
    val savedStreetEdge: StreetEdge = StreetEdgeRepository.find(edgeId)

    val sidewalkEdge1Id = SidewalkEdgeRepository.create(SidewalkEdge(4, 5, "key1", NorthSide, savedStreetEdge.id, None, isAccessible = false))
    val sidewalkEdge1 = SidewalkEdgeRepository.find(sidewalkEdge1Id)
    sidewalkEdge1.id shouldBe 'defined
    sidewalkEdge1.keyValue shouldBe "key1"
    sidewalkEdge1.vertexStartId shouldBe sidewalk1.id
    sidewalkEdge1.vertexEndId shouldBe sidewalk2.id
    sidewalkEdge1.streetEdgeBelongToId.get shouldBe savedStreetEdge.id.get
    sidewalkEdge1.streetEdgeBelongToId.get shouldBe edgeId
    sidewalkEdge1.isAccessible shouldBe false

    val sidewalkEdges = SidewalkEdgeRepository.findSidewalkEdgesBySidewalkVertex(sidewalk1.id)
    sidewalkEdges.size shouldBe 1
    sidewalkEdges.head.id.get shouldBe sidewalkEdge1Id
    sidewalkEdges.head.keyValue shouldBe "key1"
  }
}
