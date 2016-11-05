package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport.PublicTransportCombination
import mapdomain.repository.BaseRepositoryDBSpec
import mapdomain.sidewalk.Ramp
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}
import scalikejdbc.config.DBs
import utils.JsonUtil

import scala.math._

class PublicTransportCombinationRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "PublicTransportCombination Repository" should "update PublicTransportCombination correctly" in {
    var ptc1 = PublicTransportCombinationRepository.create(PublicTransportCombination(1, 2, 3, 4, 5, None, true, 1))
    var ptc2 = PublicTransportCombinationRepository.create(PublicTransportCombination(10, 11, 3, 4, 5, None, true, 1))
    Some(JsonUtil.toJson(Ramp(Coordinate(1, 2), None, "asd", false)))
    ptc2 = ptc2.copy(walkPath = Some(JsonUtil.toJson(Ramp(Coordinate(1, 2), None, "asd", false))))
    ptc1 = ptc1.copy(walkPath = Some(JsonUtil.toJson((Ramp(Coordinate(1.1233, -2.33909), None, "asd", false)))))

    PublicTransportCombinationRepository.save(ptc1)
    PublicTransportCombinationRepository.save(ptc2)

    val ptcs = PublicTransportCombinationRepository.findAll
    ptcs.size shouldBe 2
    ptcs.head.walkPath should not be None

    ptcs.last.walkPath should not be None
  }

}
