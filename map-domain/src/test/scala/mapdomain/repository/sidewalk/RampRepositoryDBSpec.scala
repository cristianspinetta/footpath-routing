package mapdomain.repository.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.repository.BaseRepositoryDBSpec
import mapdomain.sidewalk._
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
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

    RampRepository.findAll.size shouldBe 1
  }

}
