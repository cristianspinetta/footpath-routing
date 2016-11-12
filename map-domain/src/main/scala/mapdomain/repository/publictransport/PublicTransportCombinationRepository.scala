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
      fromCoordinate = coordinateFromResultSet(rs, tableAlias, "fromCoordinate"),
      toStopId = rs.long(ptc.toStopId),
      toCoordinate = coordinateFromResultSet(rs, tableAlias, "toCoordinate"),
      fromTravelInfoId = rs.long(ptc.fromTravelInfoId),
      toTravelInfoId = rs.long(ptc.toTravelInfoId),
      distance = rs.double(ptc.distance),
      enabled = rs.boolean(ptc.enabled))
  }

  def findByTravelInfoId(travelInfoId: Long)(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select
      .all(ptc)
      .append(selectLatitudeAndLongitude(ptc, "fromCoordinate"))
      .append(selectLatitudeAndLongitude(ptc, "toCoordinate"))
      .from(PublicTransportCombination as ptc)
      .where
      .eq(ptc.fromTravelInfoId, travelInfoId)
      .or
      .eq(ptc.toTravelInfoId, travelInfoId)
  }.map(publicTransportCombination(ptc)).list().apply()

  def findByMultipleTravelInfoIds(travelInfoIds: List[Long], limit: Int,
    excludedRadius: List[(Coordinate, Double)] = List.empty)(
      implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {

    select
      .all(ptc)
      .append(selectLatitudeAndLongitude(ptc, "fromCoordinate"))
      .append(selectLatitudeAndLongitude(ptc, "toCoordinate"))
      .from(PublicTransportCombination as ptc)
      .where
      .in(ptc.fromTravelInfoId, travelInfoIds)
      .map { (sql: scalikejdbc.ConditionSQLBuilder[Stop]) ⇒
        excludedRadius.foldLeft(sql) {
          case (s, (coordinate, radius)) ⇒
            s
              .and.not.withRoundBracket { _.append(clauseNearestByDistance(coordinate, radius, ptc, "fromCoordinate")) }
              .and.not.withRoundBracket { _.append(clauseNearestByDistance(coordinate, radius, ptc, "toCoordinate")) }
        }
      }
      //      .or
      //      .in(ptc.toTravelInfoId, travelInfoIds)
      .orderBy(ptc.distance).asc
      .limit(limit)
  }.map(publicTransportCombination(ptc)).list().apply()

  def findByRadius(coordinate: Coordinate, radius: Double)(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select
      .all(ptc)
      .append(selectLatitudeAndLongitude(ptc, "fromCoordinate"))
      .append(selectLatitudeAndLongitude(ptc, "toCoordinate"))
      .from(PublicTransportCombination as ptc)
      .leftJoin(Stop as s).on(ptc.fromStopId, s.id)
      .leftJoin(Stop as s2).on(ptc.toStopId, s2.id)
      .where
      .append(clauseNearestByDistance(coordinate, radius, s, "coordinate"))
      .and
      .append(clauseNearestByDistance(coordinate, radius, s2, "coordinate"))
  }.map(publicTransportCombination(ptc)).list().apply()

  def findAll(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select
      .all(ptc)
      .append(selectLatitudeAndLongitude(ptc, "fromCoordinate"))
      .append(selectLatitudeAndLongitude(ptc, "toCoordinate"))
      .from(PublicTransportCombination as ptc)
  }.map(publicTransportCombination(ptc)).list().apply()

  def findLimitted(limit: Integer, offset: Integer)(implicit session: DBSession = PublicTransportCombination.autoSession): List[PublicTransportCombination] = withSQL {
    select
      .all(ptc)
      .append(selectLatitudeAndLongitude(ptc, "fromCoordinate"))
      .append(selectLatitudeAndLongitude(ptc, "toCoordinate"))
      .from(PublicTransportCombination as ptc)
      .limit(limit)
      .offset(offset)
  }.map(publicTransportCombination(ptc)).list().apply()

  def save(ptc: PublicTransportCombination)(implicit session: DBSession = PublicTransportCombination.autoSession): PublicTransportCombination = {
    withSQL {
      update(PublicTransportCombination)
        .set(
          PublicTransportCombination.column.fromCoordinate -> positionToSQL(ptc.fromCoordinate),
          PublicTransportCombination.column.toStopId -> ptc.toStopId,
          PublicTransportCombination.column.toCoordinate -> positionToSQL(ptc.toCoordinate),
          PublicTransportCombination.column.fromTravelInfoId -> ptc.fromTravelInfoId,
          PublicTransportCombination.column.distance -> ptc.distance,
          PublicTransportCombination.column.enabled -> ptc.enabled
        )
        .where
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
        PublicTransportCombination.column.fromCoordinate -> positionToSQL(ptc.fromCoordinate),
        PublicTransportCombination.column.toStopId -> ptc.toStopId,
        PublicTransportCombination.column.toCoordinate -> positionToSQL(ptc.toCoordinate),
        PublicTransportCombination.column.fromTravelInfoId -> ptc.fromTravelInfoId,
        PublicTransportCombination.column.toTravelInfoId -> ptc.toTravelInfoId,
        PublicTransportCombination.column.distance -> ptc.distance,
        PublicTransportCombination.column.enabled -> ptc.enabled
      )
    }.update().apply()

    ptc
  }

  def deleteAll(implicit session: DBSession = PublicTransportCombination.autoSession): Unit = withSQL {
    deleteFrom(PublicTransportCombination)
  }.update.apply()

}

object PublicTransportCombinationRepository extends PublicTransportCombinationRepository