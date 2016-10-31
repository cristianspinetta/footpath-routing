package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport._
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport

trait StopRepository extends SpatialSQLSupport {

  val (s, ti, ptc) = (Stop.syntax("s"), TravelInfo.syntax("ti"), PublicTransportCombination.syntax("sc"))

  def stop(s: SyntaxProvider[Stop])(rs: WrappedResultSet): Stop = stop(s.resultName, s.tableAliasName)(rs)

  private def stop(s: ResultName[Stop], tableAlias: String)(implicit rs: WrappedResultSet): Stop = {
    Stop(
      id = rs.long(s.id),
      isAccessible = rs.boolean(s.isAccessible),
      coordinate = coordinateFromResultSet(rs, tableAlias),
      nextStopId = rs.get(s.nextStopId),
      previousStopId = rs.get(s.previousStopId),
      sequence = rs.get(s.sequence),
      pathId = rs.longOpt(s.pathId),
      travelInfoId = rs.get(s.travelInfoId))
  }

  def publicTransportCombination(s: SyntaxProvider[PublicTransportCombination])(rs: WrappedResultSet): PublicTransportCombination = publicTransportCombination(s.resultName, s.tableAliasName)(rs)

  private def publicTransportCombination(ptc: ResultName[PublicTransportCombination], tableAlias: String)(implicit rs: WrappedResultSet): PublicTransportCombination = {
    PublicTransportCombination(
      fromStopId = rs.long(ptc.fromStopId),
      toStopId = rs.long(ptc.toStopId),
      fromTravelInfoId = rs.long(ptc.fromTravelInfoId),
      toTravelInfoId = rs.long(ptc.fromTravelInfoId),
      distance = rs.double(ptc.distance),
      walkPath = rs.bytesOpt(ptc.walkPath),
      enabled = rs.boolean(ptc.enabled),
      cost = rs.double(ptc.cost))
  }

  def create(stopUnsaved: StopUnsaved)(implicit session: DBSession = Stop.autoSession): Stop = {
    val id = withSQL {
      insert.into(Stop).namedValues(
        Stop.column.coordinate -> positionToSQL(stopUnsaved.coordinate),
        Stop.column.nextStopId -> stopUnsaved.nextStopId,
        Stop.column.previousStopId -> stopUnsaved.previousStopId,
        Stop.column.sequence -> stopUnsaved.sequence,
        Stop.column.pathId -> stopUnsaved.pathId,
        Stop.column.travelInfoId -> stopUnsaved.travelInfoId,
        Stop.column.isAccessible -> stopUnsaved.isAccessible)
    }.updateAndReturnGeneratedKey().apply()

    Stop.createByUnsaved(id, stopUnsaved)
  }

  def find(id: Long)(implicit session: DBSession = Stop.autoSession): Option[Stop] = {
    withSQL {
      select.all(s)
        .append(selectLatitudeAndLongitude(s))
        .from(Stop as s)
        .where.eq(s.id, id)
    }.map(stop(s)).single().apply()
  }

  def findByTravelInfoId(travelInfoId: Long)(implicit session: DBSession = Stop.autoSession): List[Stop] = {
    withSQL {
      select.all(s)
        .append(selectLatitudeAndLongitude(s))
        .from(Stop as s)
        .where.eq(s.travelInfoId, travelInfoId)
    }.map(stop(s)).list().apply()
  }

  def findByRadiusAndLine(coordinate: Coordinate, radiusOpt: Option[Double] = None, lineOpt: Option[String] = None)(implicit session: DBSession = Stop.autoSession): List[Stop] = withSQL {
    select.all(s)
      .append(selectLatitudeAndLongitude(s))
      .from(Stop as s)
      .map { (sql: scalikejdbc.SelectSQLBuilder[Stop]) ⇒
        lineOpt.map(line ⇒ sql.leftJoin(TravelInfo as ti).on(s.travelInfoId, ti.id)).getOrElse(sql)
      }
      .where(sqls.toAndConditionOpt(
        lineOpt.map(line ⇒ sqls.like(ti.name, line)),
        radiusOpt.map(radius ⇒ clauseNearestByDistance(coordinate, radius, s, "coordinate"))))
  }.map(stop(s)).list().apply()

  def findAllCombinations()(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select.all(ptc)
      .from(PublicTransportCombination as ptc)
  }.map(publicTransportCombination(ptc)).list().apply()

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