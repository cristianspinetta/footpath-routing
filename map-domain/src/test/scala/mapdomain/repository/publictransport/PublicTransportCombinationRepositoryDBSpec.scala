package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{Path, PublicTransportCombination}
import mapdomain.repository.BaseRepositoryDBSpec
import mapdomain.sidewalk.Ramp
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}
import scalikejdbc.config.DBs
import snapshot.ObjectSerializer

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
    ptc2 = ptc2.copy(walkPath = Some(ObjectSerializer.serialize(Ramp(Coordinate(1, 2), None, "asd", false))))
    ptc1 = ptc1.copy(walkPath = Some(ObjectSerializer.serialize(Ramp(Coordinate(1, 2), None, "asd", false))))

    PublicTransportCombinationRepository.save(ptc2)
    PublicTransportCombinationRepository.save(ptc1)

    val ptcs = PublicTransportCombinationRepository.findAll
    ptcs.size shouldBe 2
    ptcs.head.walkPath should not be None

    ptcs.last.walkPath should not be None
  }

}
