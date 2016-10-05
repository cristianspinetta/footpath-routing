package mapdomain.repository.publictransport

import mapdomain.repository.BaseRepositoryDBSpec
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers }
import scalikejdbc.config.DBs

import scala.math._

class PathRepositoryDBSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach
    with BaseRepositoryDBSpec {

  val precision: Double = pow(10, -8)

  override def beforeAll(): Unit = DBs.setupAll()

  override def beforeEach(): Unit = deleteAll()

  override def afterAll(): Unit = DBs.closeAll()

  "Path Repository" should "create paths correctly" in {
    val coordinates = "{lng: 34, lat: 20}, {lng: 34, lat: 21}"
    var path = PathRepository.create(coordinates)
    path.id should not be None

    path = PathRepository.find(path.id.get).get
    path.id should not be None
    path.coordinates shouldBe coordinates
  }

}
