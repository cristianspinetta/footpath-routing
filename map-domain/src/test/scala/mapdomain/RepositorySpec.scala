package mapdomain

import mapdomain.graph.Coordinate
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}
import scalikejdbc.config.DBs
import mapdomain.graph.CoordinateRepository

class RepositorySpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  override def beforeAll() {
    DBs.setupAll()
  }

  override def afterEach(): Unit = {
    CoordinateRepository.deleteAll()
  }

  "With database configurated" should "create coordinates correctly" in {
    var coordinate: Coordinate= CoordinateRepository.create(10, 20)
    coordinate.id should not be None
    coordinate = CoordinateRepository.find(coordinate.id.get).get
    coordinate.latitude shouldBe 10
    coordinate.longitude shouldBe 20
  }

}
