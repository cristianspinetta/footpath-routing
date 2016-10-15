package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{ Path, StopUnsaved, TravelInfo, TravelInfoUnsaved }
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

    val travelInfoUnsaved: TravelInfoUnsaved = TravelInfoUnsaved(
      description = "Line 150", firstStopId = Some(1), lastStopId = Some(10),
      branch = "A", name = "Linea 123", sentido = "Forward", `type` = "BUS")
    var travelInfo: TravelInfo = TravelInfoRepository.create(travelInfoUnsaved)
    val path: Path = PathRepository.create("some coordinates")

    val firstStop = StopRepository.create(
      StopUnsaved(Coordinate(10l, 11l), sequence = 1, pathId = path.id.get,
        travelInfoId = travelInfo.id, isAccessible = false))
    val lastStop = StopRepository.create(
      StopUnsaved(Coordinate(12l, 13l), sequence = 2, pathId = path.id.get,
        travelInfoId = travelInfo.id, isAccessible = true))

    travelInfo = travelInfo.copy(firstStopId = firstStop.id, lastStopId = lastStop.id)
    TravelInfoRepository.save(travelInfo)

    travelInfo = TravelInfoRepository.find(travelInfo.id).get
    travelInfo.description shouldBe travelInfoUnsaved.description
    travelInfo.branch shouldBe travelInfoUnsaved.branch
    travelInfo.`type` shouldBe travelInfoUnsaved.`type`
    travelInfo.name shouldBe travelInfoUnsaved.name
    travelInfo.sentido shouldBe travelInfoUnsaved.sentido
    travelInfo.firstStopId shouldBe firstStop.id
    travelInfo.lastStopId shouldBe lastStop.id

    TravelInfoRepository.deleteAll
  }

}
