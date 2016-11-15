package mapdomain.repository.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.repository.BaseRepositoryDBSpec
import mapdomain.sidewalk._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}
import scalikejdbc.config.DBs

import scala.math._

class RampRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "Ramp Repository" should "create ramps correctly" in {
    var ramp: Ramp = RampRepository.create(11, 12, "Callao 523", isAccessible = false)
    ramp.id.isDefined shouldBe true
    ramp = RampRepository.find(ramp.id.get).get
    coordinateAssertion(ramp.coordinate, Coordinate(11, 12))
    ramp.address shouldBe "Callao 523"
    ramp.isAccessible shouldBe false

    var secondRamp = RampRepository.create(15, 16, "Callao 523", false)

    RampRepository.findAll.size shouldBe 2

    var nearest = RampRepository.findAssociatedRampsInRectangle(Coordinate(20, 20), Coordinate(10, 10))
    nearest.size shouldBe 0

    StreetCrossingEdgeRepository.create(StreetCrossingEdge(1, 1, "key3", None, ramp.id, secondRamp.id))
    nearest = RampRepository.findAssociatedRampsInRectangle(Coordinate(20, 20), Coordinate(10, 10))
    nearest.size shouldBe 2

    nearest = RampRepository.findAssociatedRampsInRectangle(Coordinate(20, 13), Coordinate(10, 10))
    nearest.size shouldBe 1
    nearest.head.id shouldBe ramp.id

    nearest = RampRepository.findAssociatedRampsInRectangle(Coordinate(20, 20), Coordinate(10, 13))
    nearest.head.id should not be ramp.id

    nearest = RampRepository.findAssociatedRampsInRectangle(Coordinate(60, 60), Coordinate(50, 50))
    nearest.size shouldBe 0
  }

}
