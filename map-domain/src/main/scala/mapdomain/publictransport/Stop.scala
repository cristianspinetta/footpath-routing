package mapdomain.publictransport

import mapdomain.graph.{ Coordinate, CoordinateRepository }
import scalikejdbc.{ WrappedResultSet, _ }

case class Stop(
  id: Option[Long] = None,
  coordinateId: Option[Long] = None,
  coordinate: Option[Coordinate] = None,
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

trait StopRepository {

  val s = Stop.syntax("s")

  val (c, ns, ps, p, ti) = (CoordinateRepository.c, Stop.syntax("ns"), Stop.syntax("ps"), PathRepository.p, TravelInfoRepository.ti)

  def stop(s: SyntaxProvider[Stop])(rs: WrappedResultSet): Stop = stop(s.resultName)(rs)

  private def stop(s: ResultName[Stop])(implicit rs: WrappedResultSet): Stop = {
    new Stop(
      id = Some(rs.long(s.id)),
      cellNumber = rs.int(s.cellNumber),
      isAccessible = rs.boolean(s.isAccessible),
      coordinateId = rs.get(s.coordinateId),
      nextStopId = rs.get(s.nextStopId),
      previousStopId = rs.get(s.previousStopId),
      pathId = rs.get(s.pathId),
      travelInfoId = rs.get(s.travelInfoId))
  }

  private def stop(s: SyntaxProvider[Stop], c: SyntaxProvider[Coordinate], ns: SyntaxProvider[Stop], ps: SyntaxProvider[Stop], p: SyntaxProvider[Path], ti: SyntaxProvider[TravelInfo])(rs: WrappedResultSet): Stop = {
    stop(s)(rs)
      .copy(coordinate = Some(CoordinateRepository.coordinate(c)(rs)))
      .copy(nextStop = Some(stop(ns)(rs)))
      .copy(previousStop = Some(stop(ps)(rs)))
      .copy(path = Some(PathRepository.path(p)(rs)))
      .copy(travelInfo = Some(TravelInfoRepository.travelInfo(ti)(rs)))
  }

  def create(latitude: Long, longitude: Long, cellNumber: Int, isAccessible: Boolean, pathId: Long)(implicit session: DBSession = Stop.autoSession): Stop = {
    val coordinate = CoordinateRepository.create(latitude, longitude)
    val id = withSQL {
      insert.into(Stop).namedValues(
        Stop.column.coordinateId -> coordinate.id.get,
        Stop.column.cellNumber -> cellNumber,
        Stop.column.isAccessible -> isAccessible,
        Stop.column.pathId -> pathId)
    }.updateAndReturnGeneratedKey().apply()

    new Stop(
      id = Some(id),
      coordinateId = coordinate.id,
      coordinate = Some(coordinate),
      cellNumber = cellNumber,
      pathId = Some(pathId),
      isAccessible = isAccessible)
  }

  def find(id: Long)(implicit session: DBSession = Stop.autoSession): Option[Stop] = {
    withSQL {
      select.from(Stop as s)
        .leftJoin(Coordinate as c).on(s.coordinateId, c.id)
        .leftJoin(Stop as ns).on(s.nextStopId, ns.id)
        .leftJoin(Stop as ps).on(s.previousStopId, ps.id)
        .leftJoin(Path as p).on(s.pathId, p.id)
        .leftJoin(TravelInfo as ti).on(s.travelInfoId, ti.id)
        .where.eq(s.id, id)
    }.map(stop(s, c, ns, ps, p, ti)).single().apply()
  }

  def save(stop: Stop)(implicit session: DBSession = Stop.autoSession): Stop = {
    withSQL {
      update(Stop).set(
        Stop.column.coordinateId -> stop.coordinateId,
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