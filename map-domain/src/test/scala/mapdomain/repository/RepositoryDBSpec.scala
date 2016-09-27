package mapdomain.repository

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{ PathRepository, StopRepository, TravelInfoRepository }
import mapdomain.sidewalk._
import mapdomain.street.{ StreetEdge, StreetEdgeRepository, StreetVertex, StreetVertexRepository }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
import scalikejdbc.DB
import scalikejdbc.config.DBs
import scalikejdbc._
import sqls.{ count, distinct }

import scala.math._

class RepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  val precision: Double = pow(10, -8)

  override def beforeAll() {
    DBs.setupAll()
    //    DBInitializer().start()
  }

  override def beforeEach(): Unit = {
    StopRepository.deleteAll
    StreetEdgeRepository.deleteAll
    StreetVertexRepository.deleteAll
    RampRepository.deleteAll
    PathRepository.deleteAll
    TravelInfoRepository.deleteAll
    SidewalkEdgeRepository.deleteAll
    StreetCrossingEdgeRepository.deleteAll
    SidewalkVertexRepository.deleteAll
  }

  override def afterAll(): Unit = {
    DBs.closeAll()
  }

  def coordinateAssertion(coordResult: Coordinate, coordExpect: Coordinate): Unit = {
    coordResult.latitude should equal(coordExpect.latitude +- precision)
    coordResult.longitude should equal(coordExpect.longitude +- precision)
  }

  "With database configurated" should "create ramps correctly" in {
    var ramp: Ramp = RampRepository.create(11, 12, "16", "Callao", Some(500), "Callao 523", false)
    ramp.id shouldBe "16"
    ramp = RampRepository.find(ramp.id).get
    coordinateAssertion(ramp.coordinate, Coordinate(11, 12))
    ramp.street shouldBe "Callao"
    ramp.number shouldBe Some(500)
    ramp.address shouldBe "Callao 523"
    ramp.isAccessible shouldBe false
  }

  it should "create street edges correctly" in {
    StreetVertexRepository.create(StreetVertex(5, Nil, Coordinate(12, 11)))
    StreetVertexRepository.create(StreetVertex(6, Nil, Coordinate(14, 13)))
    val streetEdge = StreetEdge(None, 5, 6, 10, 9)
    val edgeId = StreetEdgeRepository.create(streetEdge)

    val edge: StreetEdge = StreetEdgeRepository.find(edgeId)
    edge.id should not be None
    edge.distance shouldBe 10d
    edge.wayId shouldBe 9l
    edge.vertexStartId shouldBe 5
    edge.vertexEndId shouldBe 6
  }

  it should "create street vertex correctly" in {
    var vertex1 = StreetVertexRepository.create(StreetVertex(5, Nil, Coordinate(12, 11)))
    vertex1 = StreetVertexRepository.find(vertex1.id).get
    vertex1.id shouldBe 5
    coordinateAssertion(vertex1.coordinate, Coordinate(12, 11))

    val vertex2 = StreetVertexRepository.create(StreetVertex(6, Nil, Coordinate(14, 13)))
    val vertex3 = StreetVertexRepository.create(StreetVertex(7, Nil, Coordinate(14, 15)))

    StreetEdgeRepository.create(StreetEdge(None, vertex1.id, vertex2.id, 10d, 9l))
    StreetEdgeRepository.create(StreetEdge(None, vertex2.id, vertex1.id, 10d, 9l))
    StreetEdgeRepository.create(StreetEdge(None, vertex2.id, vertex3.id, 10d, 9l))
    StreetEdgeRepository.create(StreetEdge(None, vertex3.id, vertex2.id, 10d, 9l))

    var neighbours = StreetVertexRepository.findNeighbours(vertex2.id)
    neighbours.size shouldBe 2
    neighbours.sortWith(_.id < _.id)
    neighbours.head.id shouldBe vertex1.id
    neighbours.tail.head.id shouldBe vertex3.id

    neighbours = StreetVertexRepository.findNeighbours(vertex1.id)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe vertex2.id

    neighbours = StreetVertexRepository.findNeighbours(vertex3.id)
    neighbours.size shouldBe 1
    neighbours.head.id shouldBe vertex2.id
  }

  it should "create street vertex in bulk correctly" in {
    val qunatityToInsert = 15
    val vertices = (1 to qunatityToInsert).map(i ⇒ StreetVertex(i, Nil, Coordinate(i * 3, i * 5))).toList
    StreetVertexRepository.createInBulk(vertices)

    val v = StreetVertexRepository.v
    val verticesCount: Int = DB readOnly { implicit session ⇒
      withSQL {
        select(count(distinct(v.id))).from(StreetVertex as v).where.between(v.id, 1, qunatityToInsert)
      }.map(_.int(1)).single().apply().get
    }

    verticesCount shouldBe qunatityToInsert
  }

  it should "create paths correctly" in {
    val coordinates = "{lng: 34, lat: 20}, {lng: 34, lat: 21}"
    var path = PathRepository.create(coordinates)
    path.id should not be None

    path = PathRepository.find(path.id.get).get
    path.id should not be None
    path.coordinates shouldBe coordinates
  }

  it should "create stops correctly" in {
    val travelInfo = TravelInfoRepository.create("any description")
    val coordinates = "{lng: 34, lat: 20}, {lng: 34, lat: 21}"
    val path = PathRepository.create(coordinates)

    val firstStop = StopRepository.create(10l, 11l, false, path.id.get)
    var secondStop = StopRepository.create(12l, 13l, true, path.id.get)
    val thirdStop = StopRepository.create(14l, 15l, true, path.id.get)

    secondStop = secondStop.copy(previousStopId = firstStop.id, nextStopId = thirdStop.id, travelInfoId = travelInfo.id)
    StopRepository.save(secondStop)

    secondStop = StopRepository.find(secondStop.id.get).get
    secondStop.isAccessible shouldBe true
    secondStop.previousStopId shouldBe firstStop.id
    secondStop.previousStop.get.isAccessible shouldBe false
    coordinateAssertion(secondStop.coordinate, Coordinate(12, 13))
    secondStop.nextStopId shouldBe thirdStop.id
    secondStop.nextStop.get.isAccessible shouldBe true
    secondStop.path.get.coordinates shouldBe coordinates
    secondStop.pathId shouldBe path.id
    secondStop.travelInfo.get.description shouldBe travelInfo.description
    secondStop.travelInfoId shouldBe travelInfo.id

    StopRepository.deleteAll
  }

  it should "create travelInfo correctly" in {
    var travelInfo = TravelInfoRepository.create("any description")

    val path = PathRepository.create("some coordinates")
    val firstStop = StopRepository.create(10l, 11l, false, path.id.get)
    val lastStop = StopRepository.create(12l, 13l, true, path.id.get)

    travelInfo = travelInfo.copy(firstStopId = firstStop.id, lastStopId = lastStop.id)
    TravelInfoRepository.save(travelInfo)

    travelInfo = TravelInfoRepository.find(travelInfo.id.get).get
    travelInfo.description shouldBe "any description"
    travelInfo.firstStopId shouldBe firstStop.id
    travelInfo.lastStopId shouldBe lastStop.id
    travelInfo.firstStop.get.pathId shouldBe path.id
    travelInfo.lastStop.get.pathId shouldBe path.id

    TravelInfoRepository.deleteAll
  }

  it should "create sidewalks correctly" in {
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

    val ramp1 = RampRepository.create(10, 9, "1", "Callao", Some(1234), "Callao 1234", true)
    val ramp2 = RampRepository.create(10, 9, "2", "Callao", Some(1234), "Callao 1234", true)
    val ramp3 = RampRepository.create(10, 9, "3", "Callao", Some(1234), "Callao 1234", true)
    val ramp4 = RampRepository.create(10, 9, "4", "Callao", Some(1234), "Callao 1234", true)

    val streetEdge = StreetEdge(None, 5, 6, 10, 9)
    val edgeId = StreetEdgeRepository.create(streetEdge)
    val savedStreetEdge: StreetEdge = StreetEdgeRepository.find(edgeId)
    val edge1Id = SidewalkEdgeRepository.create(SidewalkEdge(sidewalk2.id, sidewalk1.id, "key1", NorthSide, savedStreetEdge.id))
    val edge2Id = SidewalkEdgeRepository.create(SidewalkEdge(sidewalk1.id, sidewalk3.id, "key2", NorthSide, savedStreetEdge.id))
    StreetCrossingEdgeRepository.create(StreetCrossingEdge(sidewalk1.id, sidewalk4.id, "key3", None, Some(ramp1.id), Some(ramp2.id)))
    val crossingEdge2Id = StreetCrossingEdgeRepository.create(StreetCrossingEdge(sidewalk5.id, sidewalk1.id, "key4", None, Some(ramp3.id), Some(ramp4.id)))

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
    edge1.isAccessible = false
    SidewalkEdgeRepository.save(edge1)
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
    edge2.isAccessible = false
    SidewalkEdgeRepository.save(edge2)
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

    val streetEdge = StreetEdge(None, 5, 6, 10, 9)
    val edgeId = StreetEdgeRepository.create(streetEdge)
    val savedStreetEdge: StreetEdge = StreetEdgeRepository.find(edgeId)

    val sidewalkEdge1Id = SidewalkEdgeRepository.create(SidewalkEdge(4, 5, "key1", NorthSide, savedStreetEdge.id, None, false))
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
