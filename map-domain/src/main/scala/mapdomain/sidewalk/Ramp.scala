package mapdomain.sidewalk

import mapdomain.graph.{ Coordinate, CoordinateRepository }
import scalikejdbc.{ DBSession, WrappedResultSet, _ }

case class Ramp(coordinate: Coordinate,
    id: String,
    street: String,
    number: Option[Int],
    address: String,
    coordinateId: Option[Long] = None) {
}

object Ramp extends SQLSyntaxSupport[Ramp] {

  override val tableName = "Ramp"

  override val columns = Seq("id", "street", "number", "address", "coordinateId")

  override val useSnakeCaseColumnName = false

}

trait RampRepository {

  val r = Ramp.syntax("r")

  private val c = CoordinateRepository.c

  private def ramp(c: SyntaxProvider[Ramp])(rs: WrappedResultSet): Ramp = ramp(c.resultName)(rs)

  private def ramp(r: ResultName[Ramp])(implicit rs: WrappedResultSet): Ramp = {
    new Ramp(
      coordinate = null,
      id = rs.string(r.id),
      street = rs.string(r.street),
      number = Some(rs.int(r.number)),
      address = rs.string(r.address))
  }

  private def rampWithCoordinate(r: SyntaxProvider[Ramp], c: SyntaxProvider[Coordinate])(rs: WrappedResultSet): Ramp = {
    ramp(r)(rs).copy(
      coordinate = CoordinateRepository.coordinate(c)(rs))
  }

  def create(latitude: Double, longitude: Double, id: String, street: String, number: Option[Int], address: String)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    val coordinate = CoordinateRepository.create(latitude, longitude)
    withSQL {
      insert.into(Ramp).namedValues(
        Ramp.column.coordinateId -> coordinate.id.get,
        Ramp.column.id -> id,
        Ramp.column.street -> street,
        Ramp.column.number -> number,
        Ramp.column.address -> address)
    }.update().apply()

    Ramp(coordinate, id, street, number, address)
  }

  def find(id: String)(implicit session: DBSession = Ramp.autoSession): Option[Ramp] = withSQL {
    select
      .from(Ramp as r)
      .leftJoin(Coordinate as c).on(r.coordinateId, c.id)
      .where.eq(r.id, id)
  }.map(rampWithCoordinate(r, c)(_)).single.apply()

  def deleteAll(implicit session: DBSession = Ramp.autoSession): Unit = withSQL {
    deleteFrom(Ramp)
  }.update.apply()

}

object RampRepository extends RampRepository
