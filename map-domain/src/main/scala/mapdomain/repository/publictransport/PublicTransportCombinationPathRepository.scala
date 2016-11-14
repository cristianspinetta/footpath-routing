package mapdomain.repository.publictransport

import mapdomain.publictransport.{ PublicTransportCombination, PublicTransportCombinationPath }
import scalikejdbc._
import sql.SpatialSQLSupport

trait PublicTransportCombinationPathRepository {

  val ptcp = PublicTransportCombinationPath.syntax("ptcp")

  def publicTransportCombinationPath(ptcp: SyntaxProvider[PublicTransportCombinationPath])(rs: WrappedResultSet): PublicTransportCombinationPath = publicTransportCombinationPath(ptcp.resultName)(rs)

  private def publicTransportCombinationPath(ptcp: ResultName[PublicTransportCombinationPath])(implicit rs: WrappedResultSet): PublicTransportCombinationPath = {
    PublicTransportCombinationPath(
      fromStopId = rs.long(ptcp.fromStopId),
      toTravelInfoId = rs.long(ptcp.toTravelInfoId),
      walkPath = rs.string(ptcp.walkPath))
  }

  def create(combinationPath: PublicTransportCombinationPath)(implicit session: DBSession = PublicTransportCombinationPath.autoSession): PublicTransportCombinationPath = {
    withSQL {
      insert.into(PublicTransportCombinationPath).namedValues(
        PublicTransportCombinationPath.column.fromStopId -> combinationPath.fromStopId,
        PublicTransportCombinationPath.column.toTravelInfoId -> combinationPath.toTravelInfoId,
        PublicTransportCombinationPath.column.walkPath -> combinationPath.walkPath)
    }.update().apply()

    combinationPath
  }

  def findByStopAndTravelInfo(fromStopId: Long, toTravelInfoId: Long)(implicit session: DBSession = PublicTransportCombinationPath.autoSession): Option[PublicTransportCombinationPath] = withSQL {
    select
      .all(ptcp)
      .from(PublicTransportCombinationPath as ptcp)
      .where
      .eq(ptcp.fromStopId, fromStopId)
      .and
      .eq(ptcp.toTravelInfoId, toTravelInfoId)
  }.map(publicTransportCombinationPath(ptcp)).single().apply()

  def findAll(implicit session: DBSession = PublicTransportCombinationPath.autoSession): List[PublicTransportCombinationPath] = withSQL {
    select
      .all(ptcp)
      .from(PublicTransportCombinationPath as ptcp)
  }.map(publicTransportCombinationPath(ptcp)).list().apply()

  def deleteAll(implicit session: DBSession = PublicTransportCombinationPath.autoSession): Unit = withSQL {
    deleteFrom(PublicTransportCombinationPath)
  }.update.apply()

}

object PublicTransportCombinationPathRepository extends PublicTransportCombinationPathRepository
