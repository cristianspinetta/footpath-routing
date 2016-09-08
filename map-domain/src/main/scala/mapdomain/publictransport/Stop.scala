package mapdomain.publictransport

import mapdomain.graph.Coordinate
import scalikejdbc.{ WrappedResultSet, _ }
import sql.SpatialSQLSupport

case class Stop(
  id: Option[Long] = None,
  coordinate: Coordinate,
  cellNumber: Int,
  nextStopId: Option[Long] = None,
  nextStop: Option[Stop] = None,
  previousStopId: Option[Long] = None,
  previousStop: Option[Stop] = None,
  pathId: Option[Long] = None,
  path: Option[Path] = None,
  travelInfoId: Option[Long] = None,
  travelInfo: Option[TravelInfo] = None,
  isAccessible: Boolean)

object Stop extends SQLSyntaxSupport[Stop] {

  override val useSnakeCaseColumnName = false

}

trait StopRepository extends SpatialSQLSupport {

  val s = Stop.syntax("s")

  val (ns, ps, p, ti) = (Stop.syntax("ns"), Stop.syntax("ps"), PathRepository.p, TravelInfoRepository.ti)

  def stop(s: SyntaxProvider[Stop])(rs: WrappedResultSet): Stop = stop(s.resultName, s.tableAliasName)(rs)

  private def stop(s: ResultName[Stop], tableAlias: String)(implicit rs: WrappedResultSet): Stop = {
    new Stop(
      id = Some(rs.long(s.id)),
      cellNumber = rs.int(s.cellNumber),
      isAccessible = rs.boolean(s.isAccessible),
      coordinate = coordinateFromResultSet(rs, tableAlias),
      nextStopId = rs.get(s.nextStopId),
      previousStopId = rs.get(s.previousStopId),
      pathId = rs.get(s.pathId),
      travelInfoId = rs.get(s.travelInfoId))
  }

  private def stop(s: SyntaxProvider[Stop], ns: SyntaxProvider[Stop], ps: SyntaxProvider[Stop], p: SyntaxProvider[Path], ti: SyntaxProvider[TravelInfo])(rs: WrappedResultSet): Stop = {
    stop(s)(rs)
      .copy(nextStop = Some(stop(ns)(rs)))
      .copy(previousStop = Some(stop(ps)(rs)))
      .copy(path = Some(PathRepository.path(p)(rs)))
      .copy(travelInfo = Some(TravelInfoRepository.travelInfo(ti)(rs)))
  }

  def create(latitude: Long, longitude: Long, cellNumber: Int, isAccessible: Boolean, pathId: Long)(implicit session: DBSession = Stop.autoSession): Stop = {
    val coordinate = Coordinate(latitude, longitude)
    val id = withSQL {
      insert.into(Stop).namedValues(
        Stop.column.coordinate -> positionToSQL(coordinate),
        Stop.column.cellNumber -> cellNumber,
        Stop.column.isAccessible -> isAccessible,
        Stop.column.pathId -> pathId)
    }.updateAndReturnGeneratedKey().apply()

    new Stop(
      id = Some(id),
      coordinate = coordinate,
      cellNumber = cellNumber,
      pathId = Some(pathId),
      isAccessible = isAccessible)
  }

  def find(id: Long)(implicit session: DBSession = Stop.autoSession): Option[Stop] = {
    withSQL {
      select.all(s, ns, ps, p, ti)
        .append(selectLatitudeAndLongitude(s))
        .append(selectLatitudeAndLongitude(ns))
        .append(selectLatitudeAndLongitude(ps))
        .from(Stop as s)
        .leftJoin(Stop as ns).on(s.nextStopId, ns.id)
        .leftJoin(Stop as ps).on(s.previousStopId, ps.id)
        .leftJoin(Path as p).on(s.pathId, p.id)
        .leftJoin(TravelInfo as ti).on(s.travelInfoId, ti.id)
        .where.eq(s.id, id)
    }.map(stop(s, ns, ps, p, ti)).single().apply()
  }

  def save(stop: Stop)(implicit session: DBSession = Stop.autoSession): Stop = {
    withSQL {
      update(Stop).set(
        Stop.column.coordinate -> positionToSQL(stop.coordinate),
        Stop.column.cellNumber -> stop.cellNumber,
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