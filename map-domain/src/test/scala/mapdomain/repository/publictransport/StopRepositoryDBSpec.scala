package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
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
    val travelInfo = TravelInfoRepository.create("any description")
    val coordinates = "{lng: 34, lat: 20}, {lng: 34, lat: 21}"
    val path = PathRepository.create(coordinates)

    val firstStop = StopRepository.create(10l, 11l, isAccessible = false, path.id.get)
    var secondStop = StopRepository.create(12l, 13l, isAccessible = true, path.id.get)
    val thirdStop = StopRepository.create(14l, 15l, isAccessible = true, path.id.get)

    secondStop = secondStop.copy(previousStopId = firstStop.id, nextStopId = thirdStop.id, travelInfoId = travelInfo.id)
    StopRepository.save(secondStop)

    secondStop = StopRepository.find(secondStop.id.get).get
    secondStop.isAccessible shouldBe true
    secondStop.previousStopId shouldBe firstStop.id
    secondStop.previousStop.get.isAccessible shouldBe false
    coordinateAssertion(secondStop.coordinate, Coordinate(12, 13))
    secondStop.nextStopId shouldBe thirdStop.id
    secondStop.nextStop.get.isAccessible shouldBe true
    secondStop.path.get.coordinates shouldBe coordinates
    secondStop.pathId shouldBe path.id
    secondStop.travelInfo.get.description shouldBe travelInfo.description
    secondStop.travelInfoId shouldBe travelInfo.id

    StopRepository.deleteAll
  }

}
