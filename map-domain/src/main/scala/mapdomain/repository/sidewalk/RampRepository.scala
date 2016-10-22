package mapdomain.repository.sidewalk

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.{Ramp, StreetCrossingEdge}
import scalikejdbc.{DBSession, WrappedResultSet, _}
import sql.SpatialSQLSupport

trait RampRepository extends SpatialSQLSupport {

  val r = Ramp.syntax("r")
  val sce = StreetCrossingEdge.syntax("sce")

  private def ramp(c: SyntaxProvider[Ramp])(rs: WrappedResultSet): Ramp = ramp(c.resultName)(rs)

  private def ramp(resultName: ResultName[Ramp])(implicit rs: WrappedResultSet): Ramp = {
    new Ramp(
      coordinate = coordinateFromResultSet(rs, r.tableAliasName),
      id = rs.longOpt(resultName.id),
      address = rs.string(resultName.address),
      isAccessible = rs.boolean(resultName.isAccessible))
  }

  def createRamp(ramp: Ramp)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    withSQL {
      insert.into(Ramp).namedValues(
        Ramp.column.coordinate -> positionToSQL(ramp.coordinate),
        Ramp.column.address -> ramp.address,
        Ramp.column.isAccessible -> ramp.isAccessible)
    }.update().apply()

    ramp
  }

  def create(latitude: Double, longitude: Double, address: String, isAccessible: Boolean)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    val coordinate = Coordinate(latitude, longitude)
    val id = withSQL {
      insert.into(Ramp).namedValues(
        Ramp.column.coordinate -> positionToSQL(coordinate),
        Ramp.column.address -> address,
        Ramp.column.isAccessible -> isAccessible)
    }.updateAndReturnGeneratedKey().apply()

    Ramp(coordinate, Some(id), address, isAccessible)
  }

  def save(ramp: Ramp)(implicit session: DBSession = Ramp.autoSession): Ramp = {
    withSQL {
      update(Ramp).set(
        Ramp.column.address -> ramp.address,
        Ramp.column.isAccessible -> ramp.isAccessible,
        Ramp.column.coordinate -> positionToSQL(ramp.coordinate)
      ).where.eq(Ramp.column.id, ramp.id)
    }.update().apply()

    ramp
  }

  def find(id: Long)(implicit session: DBSession = Ramp.autoSession): Option[Ramp] = withSQL {
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
        .join(StreetCrossingEdge as sce).on(sqls"${sce.rampStartId} = ${r.id} or ${sce.rampEndId} = ${r.id}")
      .where.append(clauseNearestByDistance(coordinate, radius, r, "coordinate"))
  }.map(ramp(r)(_)).list().apply()

  def findRampsInRectangle(northEast: Coordinate, southWest: Coordinate)(implicit session: DBSession = Ramp.autoSession): List[Ramp] = withSQL {
    select(r.resultAll)
      .append(selectLatitudeAndLongitude(r))
      .from(Ramp as r)
      .where.append(clauseGetElementsInRectangle(northEast, southWest, r, "coordinate"))
  }.map(ramp(r)(_)).list().apply()

}

object RampRepository extends RampRepository