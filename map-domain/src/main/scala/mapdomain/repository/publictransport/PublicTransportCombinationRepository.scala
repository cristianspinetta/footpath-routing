package mapdomain.repository.publictransport

import mapdomain.graph.Coordinate
import mapdomain.publictransport._
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport

trait PublicTransportCombinationRepository extends SpatialSQLSupport {

  val (ptc, s, s2) = (PublicTransportCombination.syntax("sc"), Stop.syntax("s"), Stop.syntax("s2"))

  def publicTransportCombination(ptc: SyntaxProvider[PublicTransportCombination])(rs: WrappedResultSet): PublicTransportCombination = publicTransportCombination(ptc.resultName, ptc.tableAliasName)(rs)

  private def publicTransportCombination(ptc: ResultName[PublicTransportCombination], tableAlias: String)(implicit rs: WrappedResultSet): PublicTransportCombination = {
    PublicTransportCombination(
      fromStopId = rs.long(ptc.fromStopId),
      toStopId = rs.long(ptc.toStopId),
      fromTravelInfoId = rs.long(ptc.fromTravelInfoId),
      toTravelInfoId = rs.long(ptc.toTravelInfoId),
      distance = rs.double(ptc.distance),
      walkPath = rs.bytesOpt(ptc.walkPath),
      enabled = rs.boolean(ptc.enabled),
      cost = rs.double(ptc.cost))
  }

  def findByTravelInfoId(travelInfoId: Long)(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select.all(ptc)
      .from(PublicTransportCombination as ptc)
      .where
      .eq(ptc.fromTravelInfoId, travelInfoId)
      .or
      .eq(ptc.toTravelInfoId, travelInfoId)
  }.map(publicTransportCombination(ptc)).list().apply()

  def findByMultipleTravelInfoIds(travelInfoIds: List[Long], limit: Int)(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select.all(ptc)
      .from(PublicTransportCombination as ptc)
      .where
      .in(ptc.fromTravelInfoId, travelInfoIds)
      .or
      .in(ptc.toTravelInfoId, travelInfoIds)
      .orderBy(ptc.distance).asc
      .limit(limit)
  }.map(publicTransportCombination(ptc)).list().apply()

  def findByRadius(coordinate: Coordinate, radius: Double)(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select.all(ptc)
      .from(PublicTransportCombination as ptc)
      .leftJoin(Stop as s).on(ptc.fromStopId, s.id)
      .leftJoin(Stop as s2).on(ptc.toStopId, s2.id)
      .where
      .append(clauseNearestByDistance(coordinate, radius, s, "coordinate"))
      .and
      .append(clauseNearestByDistance(coordinate, radius, s2, "coordinate"))
  }.map(publicTransportCombination(ptc)).list().apply()

  def findAll(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select.all(ptc)
      .from(PublicTransportCombination as ptc)
  }.map(publicTransportCombination(ptc)).list().apply()

  def save(ptc: PublicTransportCombination)(implicit session: DBSession = PublicTransportCombination.autoSession): PublicTransportCombination = {
    withSQL {
      update(PublicTransportCombination).set(
        PublicTransportCombination.column.toStopId -> ptc.toStopId,
        PublicTransportCombination.column.fromTravelInfoId -> ptc.fromTravelInfoId,
        PublicTransportCombination.column.distance -> ptc.distance,
        PublicTransportCombination.column.walkPath -> ptc.walkPath,
        PublicTransportCombination.column.enabled -> ptc.enabled,
        PublicTransportCombination.column.cost -> ptc.cost
      ).where
        .eq(PublicTransportCombination.column.fromStopId, ptc.fromStopId)
        .and
        .eq(PublicTransportCombination.column.toTravelInfoId, ptc.toTravelInfoId)
    }.update().apply()

    ptc
  }

  def create(ptc: PublicTransportCombination)(implicit session: DBSession = PublicTransportCombination.autoSession): PublicTransportCombination = {
    withSQL {
      insert.into(PublicTransportCombination).namedValues(
        PublicTransportCombination.column.fromStopId -> ptc.fromStopId,
        PublicTransportCombination.column.toStopId -> ptc.toStopId,
        PublicTransportCombination.column.fromTravelInfoId -> ptc.fromTravelInfoId,
        PublicTransportCombination.column.toTravelInfoId -> ptc.toTravelInfoId,
        PublicTransportCombination.column.distance -> ptc.distance,
        PublicTransportCombination.column.walkPath -> ptc.walkPath,
        PublicTransportCombination.column.enabled -> ptc.enabled,
        PublicTransportCombination.column.cost -> ptc.cost)
    }.update().apply()

    ptc
  }

  def deleteAll(implicit session: DBSession = PublicTransportCombination.autoSession): Unit = withSQL {
    deleteFrom(PublicTransportCombination)
  }.update.apply()

}

object PublicTransportCombinationRepository extends PublicTransportCombinationRepository