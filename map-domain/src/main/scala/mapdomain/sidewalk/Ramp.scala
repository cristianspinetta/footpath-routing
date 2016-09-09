package mapdomain.sidewalk

import mapdomain.graph.Coordinate
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport

case class Ramp(
    coordinate: Coordinate,
    id: String,
    street: String,
    number: Option[Int],
    address: String) {
}

object Ramp extends SQLSyntaxSupport[Ramp] {

  override val tableName = "ramp"

  override val columns = Seq("id", "street", "number", "address", "coordinate")

  override val useSnakeCaseColumnName = false

}

trait RampRepository extends SpatialSQLSupport {

  val r = Ramp.syntax("r")

  private def ramp(c: SyntaxProvider[Ramp])(rs: WrappedResultSet): Ramp = ramp(c.resultName)(rs)

  private def ramp(r: ResultName[Ramp])(implicit rs: WrappedResultSet): Ramp = {
    new Ramp(
      coordinate = Coordinate(rs.double("lat"), rs.double("lng")),
      id = rs.string(r.id),
      street = rs.string(r.street),
      number = Some(rs.int(r.number)),
      address = rs.string(r.address))
  }

  def create(latitude: Double, longitude: Double, id: String, street: String, number: Option[Int], address: String)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    val coordinate = Coordinate(latitude, longitude)
    withSQL {
      insert.into(Ramp).namedValues(
        Ramp.column.coordinate -> positionToSQL(coordinate),
        Ramp.column.id -> id,
        Ramp.column.street -> street,
        Ramp.column.number -> number,
        Ramp.column.address -> address)
    }.update().apply()

    Ramp(coordinate, id, street, number, address)
  }

  def find(id: String)(implicit session: DBSession = Ramp.autoSession): Option[Ramp] = withSQL {
    select(r.resultAll).append(sqls", x(${r.column("coordinate")}) lng, y(${r.column("coordinate")}) lat")
      .from(Ramp as r)
      .where.eq(r.id, id)
  }.map(ramp(r)(_)).single.apply()

  def deleteAll(implicit session: DBSession = Ramp.autoSession): Unit = withSQL {
    deleteFrom(Ramp)
  }.update.apply()

}

object RampRepository extends RampRepository
