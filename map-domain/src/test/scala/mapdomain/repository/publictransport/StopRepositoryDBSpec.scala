package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{ Path, StopUnsaved, TravelInfo }
import mapdomain.repository.BaseRepositoryDBSpec
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
import scalikejdbc.config.DBs

import scala.math._

class StopRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "Stop Repository" should "create stops correctly" in {
    val travelInfo: TravelInfo = TravelInfoRepository.create("any description")
    val coordinates = "{lng: 34, lat: 20}, {lng: 34, lat: 21}"
    val path: Path = PathRepository.create(coordinates)

    val firstStop = StopRepository.create(StopUnsaved(Coordinate(10l, 11l), sequence = 1,
      pathId = path.id.get, travelInfoId = travelInfo.id.get, isAccessible = false))
    val secondStop = StopRepository.create(StopUnsaved(Coordinate(12l, 13l), sequence = 2,
      pathId = path.id.get, travelInfoId = travelInfo.id.get, isAccessible = true))
    val thirdStop = StopRepository.create(StopUnsaved(Coordinate(14l, 15l), sequence = 3,
      pathId = path.id.get, travelInfoId = travelInfo.id.get, isAccessible = true))

    val updatedSecondStop = secondStop.copy(previousStopId = Some(firstStop.id), nextStopId = Some(thirdStop.id), travelInfoId = travelInfo.id.get)
    StopRepository.save(updatedSecondStop)

    val secondStopFromDB = StopRepository.find(updatedSecondStop.id).get
    secondStopFromDB.isAccessible shouldBe true
    secondStopFromDB.previousStopId shouldBe Some(firstStop.id)
    coordinateAssertion(secondStopFromDB.coordinate, Coordinate(12, 13))
    secondStopFromDB.nextStopId shouldBe Some(thirdStop.id)
    Some(secondStopFromDB.pathId) shouldBe path.id
    Some(secondStopFromDB.travelInfoId) shouldBe travelInfo.id

    StopRepository.deleteAll
  }

}
