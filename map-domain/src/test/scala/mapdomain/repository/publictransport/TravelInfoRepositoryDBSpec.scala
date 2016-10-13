package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{ Path, StopUnsaved, TravelInfo }
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

    var travelInfo: TravelInfo = TravelInfoRepository.create("any description")
    val path: Path = PathRepository.create("some coordinates")

    val firstStop = StopRepository.create(
      StopUnsaved(Coordinate(10l, 11l), sequence = 1, pathId = path.id.get,
        travelInfoId = travelInfo.id.get, isAccessible = false))
    val lastStop = StopRepository.create(
      StopUnsaved(Coordinate(12l, 13l), sequence = 2, pathId = path.id.get,
        travelInfoId = travelInfo.id.get, isAccessible = true))

    travelInfo = travelInfo.copy(firstStopId = Some(firstStop.id), lastStopId = Some(lastStop.id))
    TravelInfoRepository.save(travelInfo)

    travelInfo = TravelInfoRepository.find(travelInfo.id.get).get
    travelInfo.description shouldBe "any description"
    travelInfo.firstStopId shouldBe Some(firstStop.id)
    travelInfo.lastStopId shouldBe Some(lastStop.id)

    TravelInfoRepository.deleteAll
  }

}
