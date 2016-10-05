package mapdomain.repository.street

import mapdomain.graph.Coordinate
import mapdomain.repository.BaseRepositoryDBSpec
import mapdomain.street._
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
import scalikejdbc.config.DBs
import scalikejdbc.{ DB, _ }
import sqls.{ count, distinct }

import scala.math._

class StreetRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "Street Repository" should "create street edges correctly" in {
    StreetVertexRepository.create(StreetVertex(5, Nil, Coordinate(12, 11)))
    StreetVertexRepository.create(StreetVertex(6, Nil, Coordinate(14, 13)))
    val wayId: Long = 2
    val streetInfoId = StreetInfoRepository.create(StreetInfo(None, Some("some address"), wayId))
    val streetEdge = StreetEdge(id = None, streetVertexStartId = 5, streetVertexEndId = 6, distance = 10, wayId = wayId, streetInfoId = streetInfoId)
    val edgeId = StreetEdgeRepository.create(streetEdge)

    val edge: StreetEdge = StreetEdgeRepository.find(edgeId)
    edge.id should not be None
    edge.distance shouldBe 10d
    edge.wayId shouldBe wayId
    edge.vertexStartId shouldBe 5
    edge.vertexEndId shouldBe 6
  }

  it should "create street vertex correctly" in {
    var vertex1: StreetVertex.T = StreetVertexRepository.create(StreetVertex(5, Nil, Coordinate(12, 11)))
    vertex1 = StreetVertexRepository.find(vertex1.id).get
    vertex1.id shouldBe 5
    coordinateAssertion(vertex1.coordinate, Coordinate(12, 11))

    val vertex2 = StreetVertexRepository.create(StreetVertex(6, Nil, Coordinate(14, 13)))
    val vertex3 = StreetVertexRepository.create(StreetVertex(7, Nil, Coordinate(14, 15)))

    val wayId: Long = 2
    val streetInfoId = StreetInfoRepository.create(StreetInfo(None, Some("an address"), wayId))

    StreetEdgeRepository.create(StreetEdge(None, vertex1.id, vertex2.id, 10d, wayId, streetInfoId))
    StreetEdgeRepository.create(StreetEdge(None, vertex2.id, vertex1.id, 10d, wayId, streetInfoId))
    StreetEdgeRepository.create(StreetEdge(None, vertex2.id, vertex3.id, 10d, wayId, streetInfoId))
    StreetEdgeRepository.create(StreetEdge(None, vertex3.id, vertex2.id, 10d, wayId, streetInfoId))

    var neighbours = StreetVertexRepository.findNeighbours(vertex2.id)
    neighbours.size shouldBe 2
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
    val vertices = (1 to qunatityToInsert).map(i ⇒ StreetVertex[StreetEdge](i, Nil, Coordinate(i * 3, i * 5))).toList
    StreetVertexRepository.createInBulk(vertices)

    val v = StreetVertexRepository.v
    val verticesCount: Int = DB readOnly { implicit session ⇒
      withSQL {
        select(count(distinct(v.id))).from(StreetVertex as v).where.between(v.id, 1, qunatityToInsert)
      }.map(_.int(1)).single().apply().get
    }

    verticesCount shouldBe qunatityToInsert
  }

  it should "create street info correctly" in {
    val unsavedStreetInfo: StreetInfo = StreetInfo(None, Some("an address"), 2)
    val streetInfoId = StreetInfoRepository.create(unsavedStreetInfo)
    val streetInfo: StreetInfo = StreetInfoRepository.find(streetInfoId)

    streetInfo.id shouldBe Some(streetInfoId)
    streetInfo.address shouldBe unsavedStreetInfo.address
    streetInfo.wayId shouldBe unsavedStreetInfo.wayId
  }

  it should "find a street info by a street edge" in {
    StreetVertexRepository.create(StreetVertex(5, Nil, Coordinate(12, 11)))
    StreetVertexRepository.create(StreetVertex(6, Nil, Coordinate(14, 13)))
    val unsavedStreetInfo: StreetInfo = StreetInfo(None, Some("some address"), 2)
    val streetInfoId = StreetInfoRepository.create(unsavedStreetInfo)
    val streetEdge = StreetEdge(id = None, streetVertexStartId = 5, streetVertexEndId = 6, distance = 10, wayId = unsavedStreetInfo.wayId, streetInfoId = streetInfoId)
    val streetEdgeId = StreetEdgeRepository.create(streetEdge)

    val streetInfo: StreetInfo = StreetInfoRepository.findByStreetEdge(streetEdgeId)

    streetInfo.id shouldBe Some(streetInfoId)
    streetInfo.address shouldBe unsavedStreetInfo.address
    streetInfo.wayId shouldBe unsavedStreetInfo.wayId
  }
}
