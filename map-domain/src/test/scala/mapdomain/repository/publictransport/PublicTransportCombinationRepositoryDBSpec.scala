package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{PublicTransportCombination, PublicTransportCombinationPath}
import mapdomain.repository.BaseRepositoryDBSpec
import mapdomain.sidewalk.Ramp
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}
import scalikejdbc.config.DBs

import scala.math._

class PublicTransportCombinationRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "PublicTransportCombinationPath Repository" should "create PublicTransportCombinationPaths correctly" in {
    val ptcp1 = PublicTransportCombinationPathRepository.create(PublicTransportCombinationPath(1, 2, "test-walkPath-1"))
    val ptcp2 = PublicTransportCombinationPathRepository.create(PublicTransportCombinationPath(3, 4, "test-walkPath-2"))

    val ptcs = PublicTransportCombinationPathRepository.findAll
    ptcs.size shouldBe 2

    assertWalkPath(ptcp1)
    assertWalkPath(ptcp2)

    val ptcp = PublicTransportCombinationPathRepository.findByStopAndTravelInfo(50, 50)
    ptcp.isDefined shouldBe false
  }

  def assertWalkPath(combinationPath: PublicTransportCombinationPath): Unit = {
    val ptcp = PublicTransportCombinationPathRepository.findByStopAndTravelInfo(combinationPath.fromStopId, combinationPath.toTravelInfoId)
    ptcp.isDefined shouldBe true
    ptcp.get.fromStopId shouldBe combinationPath.fromStopId
    ptcp.get.toTravelInfoId shouldBe combinationPath.toTravelInfoId
    ptcp.get.walkPath shouldBe combinationPath.walkPath
  }

}
