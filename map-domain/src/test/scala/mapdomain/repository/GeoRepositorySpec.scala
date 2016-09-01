package mapdomain.repository

import mapdomain.graph.{BaseEntity, Coordinate}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}
import scalikejdbc.{DBSession, WrappedResultSet, _}
import scalikejdbc.config.DBs

class GeoRepositorySpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  override def beforeAll() {
    DBs.setupAll()
    initializeDB()
//    new DBInitializer().start()
  }

  override def afterEach(): Unit = {
  }

  override def afterAll(): Unit = {
    DBs.closeAll()
  }

  "With database configurated" should "find a geo point correctly" in {
    val spatialOpt: Option[Spatial] = SpatialRepository.findNearest(Coordinate(20, 20), 100)
    spatialOpt shouldBe 'defined
    spatialOpt.get.point.latitude should be > 0D
    spatialOpt.get.point.longitude should be > 0D
  }

  it should "create a geo point correctly" in {
    val spatial = Spatial(Coordinate(23.4, 56))
    val newSpatial = SpatialRepository.create(spatial)
    newSpatial.id shouldBe 'defined
  }

  def initializeDB() = DB autoCommit { implicit s ⇒
    sql"""
              CREATE TABLE IF NOT EXISTS `geo_test` (
                `id` bigint(20) NOT NULL AUTO_INCREMENT,
                `point` Point,
                PRIMARY KEY (`id`)
               );
   """.execute.apply()
  }

}

case class Spatial(point: Coordinate, override val id: Option[Long] = None) extends BaseEntity



object Spatial extends SQLSyntaxSupport[Spatial] {
  override val tableName = "geo_test"
}

object SpatialRepository {

  val s = Spatial.syntax("s")

  def spatial(c: SyntaxProvider[Spatial])(rs: WrappedResultSet): Spatial = spatial(rs)

  private def spatial(implicit rs: WrappedResultSet): Spatial = {
    Spatial(Coordinate(rs.double("lat"), rs.double("lng")))
  }

  def create(spatial: Spatial)(implicit session: DBSession = Spatial.autoSession): Spatial = {
    val id = withSQL {
//      insert.into(Spatial).columns(sqls"point").values(sqls"PointFromText('POINT(10 20)')")

      insert.into(Spatial).namedValues(
        Spatial.column.point -> sqls"PointFromText('POINT(10 23.6)')")

    }.updateAndReturnGeneratedKey().apply()

    spatial.copy(id = Some(id))
  }

  def findNearest(coordinate: Coordinate, distance: Double)(implicit session: DBSession = Spatial.autoSession): Option[Spatial] = {
//    val getPoints = sql"select x(point) lat, y(point) lng from geo_test where ST_Distance(point(20,20), point) <= 100 limit 1"
//    getPoints.map(spatial(s)).first().apply()
    withSQL {
      select(sqls"x(point) lat, y(point) lng").from(Spatial as s)
      .where.append(sqls"ST_Distance(point(${coordinate.latitude},${coordinate.longitude}), point) < $distance").limit(1)
    }.map(spatial(s)).first().apply()
  }

//  def save(coord: Coordinate)(implicit session: DBSession = Coordinate.autoSession): Coordinate = {
//    withSQL {
//      update(Coordinate).set(
//        Coordinate.column.latitude -> coord.latitude,
//        Coordinate.column.longitude -> coord.longitude).where.eq(Coordinate.column.id, coord.id)
//    }.update.apply()
//    coord
//  }

//  def delete(coord: Coordinate)(implicit session: DBSession = Coordinate.autoSession): Unit = withSQL {
//    deleteFrom(Coordinate).where.eq(Coordinate.column.id, coord.id)
//  }.update.apply()

//  def deleteAll(implicit session: DBSession = Coordinate.autoSession): Unit = withSQL {
//    deleteFrom(Coordinate)
//  }.update.apply()

}
