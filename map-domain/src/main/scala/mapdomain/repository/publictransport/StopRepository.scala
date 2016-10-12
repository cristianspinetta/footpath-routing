package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport.{ Path, Stop, TravelInfo }
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport

trait StopRepository extends SpatialSQLSupport {

  val s = Stop.syntax("s")

  def stop(s: SyntaxProvider[Stop])(rs: WrappedResultSet): Stop = stop(s.resultName, s.tableAliasName)(rs)

  private def stop(s: ResultName[Stop], tableAlias: String)(implicit rs: WrappedResultSet): Stop = {
    new Stop(
      id = Some(rs.long(s.id)),
      isAccessible = rs.boolean(s.isAccessible),
      coordinate = coordinateFromResultSet(rs, tableAlias),
      nextStopId = rs.get(s.nextStopId),
      previousStopId = rs.get(s.previousStopId),
      pathId = rs.get(s.pathId),
      travelInfoId = rs.get(s.travelInfoId))
  }

  def create(latitude: Long, longitude: Long, isAccessible: Boolean, pathId: Long)(implicit session: DBSession = Stop.autoSession): Stop = {
    val coordinate = Coordinate(latitude, longitude)
    val id = withSQL {
      insert.into(Stop).namedValues(
        Stop.column.coordinate -> positionToSQL(coordinate),
        Stop.column.isAccessible -> isAccessible,
        Stop.column.pathId -> pathId)
    }.updateAndReturnGeneratedKey().apply()

    new Stop(
      id = Some(id),
      coordinate = coordinate,
      pathId = Some(pathId),
      isAccessible = isAccessible)
  }

  def find(id: Long)(implicit session: DBSession = Stop.autoSession): Option[Stop] = {
    withSQL {
      select.all(s)
        .append(selectLatitudeAndLongitude(s))
        .from(Stop as s)
        .where.eq(s.id, id)
    }.map(stop(s)).single().apply()
  }

  def findNearestStops(coordinate: Coordinate, radius: Double)(implicit session: DBSession = Stop.autoSession): List[Stop] = withSQL {
    select.all(s)
      .append(selectLatitudeAndLongitude(s))
      .from(Stop as s)
      .where.append(clauseNearestByDistance(coordinate, radius, s, "coordinate"))
  }.map(stop(s)).list().apply()

  def save(stop: Stop)(implicit session: DBSession = Stop.autoSession): Stop = {
    withSQL {
      update(Stop).set(
        Stop.column.coordinate -> positionToSQL(stop.coordinate),
        Stop.column.nextStopId -> stop.nextStopId,
        Stop.column.previousStopId -> stop.previousStopId,
        Stop.column.pathId -> stop.pathId,
        Stop.column.travelInfoId -> stop.travelInfoId,
        Stop.column.isAccessible -> stop.isAccessible).where.eq(Stop.column.id, stop.id)
    }.update.apply()
    stop
  }

  def deleteAll(implicit session: DBSession = Stop.autoSession): Unit = withSQL {
    deleteFrom(Stop)
  }.update.apply()

}

object StopRepository extends StopRepository