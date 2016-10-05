package mapdomain.repository.publictransport

import mapdomain.repository.BaseRepositoryDBSpec
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
import scalikejdbc.config.DBs

import scala.math._

class TravelInfoRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "Travel Info Repository" should "create travelInfo correctly" in {
    var travelInfo = TravelInfoRepository.create("any description")

    val path = PathRepository.create("some coordinates")
    val firstStop = StopRepository.create(10l, 11l, isAccessible = false, path.id.get)
    val lastStop = StopRepository.create(12l, 13l, isAccessible = true, path.id.get)

    travelInfo = travelInfo.copy(firstStopId = firstStop.id, lastStopId = lastStop.id)
    TravelInfoRepository.save(travelInfo)

    travelInfo = TravelInfoRepository.find(travelInfo.id.get).get
    travelInfo.description shouldBe "any description"
    travelInfo.firstStopId shouldBe firstStop.id
    travelInfo.lastStopId shouldBe lastStop.id

    TravelInfoRepository.deleteAll
  }

}
