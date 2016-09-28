package mapdomain.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.street.{ StreetEdge, StreetVertex }
import scalikejdbc._
import sql.SpatialSQLSupport

case class Ramp(
    coordinate: Coordinate,
    id: String,
    street: String,
    number: Option[Int],
    address: String,
    var isAccessible: Boolean = true) {
}

object Ramp extends SQLSyntaxSupport[Ramp] {

  override val tableName = "ramp"

  override val columns = Seq("id", "street", "number", "address", "coordinate", "isAccessible")

  override val useSnakeCaseColumnName = false

}

trait RampRepository extends SpatialSQLSupport {

  val r = Ramp.syntax("r")

  private def ramp(c: SyntaxProvider[Ramp])(rs: WrappedResultSet): Ramp = ramp(c.resultName)(rs)

  private def ramp(resultName: ResultName[Ramp])(implicit rs: WrappedResultSet): Ramp = {
    new Ramp(
      coordinate = coordinateFromResultSet(rs, r.tableAliasName),
      id = rs.string(resultName.id),
      street = rs.string(resultName.street),
      number = Some(rs.int(resultName.number)),
      address = rs.string(resultName.address),
      isAccessible = rs.boolean(resultName.isAccessible))
  }

  def createRamp(ramp: Ramp)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    withSQL {
      insert.into(Ramp).namedValues(
        Ramp.column.coordinate -> positionToSQL(ramp.coordinate),
        Ramp.column.id -> ramp.id,
        Ramp.column.street -> ramp.street,
        Ramp.column.number -> ramp.number,
        Ramp.column.address -> ramp.address,
        Ramp.column.isAccessible -> ramp.isAccessible)
    }.update().apply()

    ramp
  }

  def create(latitude: Double, longitude: Double, id: String, street: String, number: Option[Int], address: String, isAccessible: Boolean)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    val coordinate = Coordinate(latitude, longitude)
    withSQL {
      insert.into(Ramp).namedValues(
        Ramp.column.coordinate -> positionToSQL(coordinate),
        Ramp.column.id -> id,
        Ramp.column.street -> street,
        Ramp.column.number -> number,
        Ramp.column.address -> address,
        Ramp.column.isAccessible -> isAccessible)
    }.update().apply()

    Ramp(coordinate, id, street, number, address, isAccessible)
  }

  def save(ramp: Ramp)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    withSQL {
      update(Ramp).set(
        Ramp.column.street -> ramp.street,
        Ramp.column.number -> ramp.number,
        Ramp.column.address -> ramp.address,
        Ramp.column.isAccessible -> ramp.isAccessible).where.eq(Ramp.column.id, ramp.id)
    }.update().apply()

    ramp
  }

  def find(id: String)(implicit session: DBSession = Ramp.autoSession): Option[Ramp] = withSQL {
    select(r.resultAll)
      .append(selectLatitudeAndLongitude(r))
      .from(Ramp as r)
      .where.eq(r.id, id)
  }.map(ramp(r)(_)).single.apply()

  def deleteAll(implicit session: DBSession = Ramp.autoSession): Unit = withSQL {
    deleteFrom(Ramp)
  }.update.apply()

  def findAll(implicit session: DBSession = Ramp.autoSession): List[Ramp] = withSQL {
    select(r.resultAll)
      .append(selectLatitudeAndLongitude(r))
      .from(Ramp as r)
  }.map(ramp(r)(_)).list().apply()

  def findNearestRamps(coordinate: Coordinate, radius: Double)(implicit session: DBSession = Ramp.autoSession): List[Ramp] = withSQL {
    select(r.resultAll)
      .append(selectLatitudeAndLongitude(r))
      .from(Ramp as r)
      .where.append(clauseNearestByDistance(coordinate, radius, r, "coordinate"))
  }.map(ramp(r)(_)).list().apply()

}

object RampRepository extends RampRepository
