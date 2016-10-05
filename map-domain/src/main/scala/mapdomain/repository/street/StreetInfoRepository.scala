package mapdomain.repository.street

import mapdomain.street.{ StreetEdge, StreetInfo }
import scalikejdbc._
import sql.SpatialSQLSupport

trait StreetInfoRepository extends SpatialSQLSupport {

  val (si, se) = (StreetInfo.syntax("si"), StreetEdge.syntax("se"))

  def streetInfo(si: SyntaxProvider[StreetInfo])(rs: WrappedResultSet): StreetInfo = streetInfo(si.resultName)(rs)

  private def streetInfo(si: ResultName[StreetInfo])(implicit rs: WrappedResultSet): StreetInfo = {
    new StreetInfo(
      Some(rs.long(si.id)),
      rs.stringOpt(si.address),
      rs.long(si.wayId))
  }

  def find(id: Long)(implicit session: DBSession = StreetInfo.autoSession): StreetInfo = withSQL {
    select.
      from(StreetInfo as si)
      .where.eq(si.id, id)
  }.map(streetInfo(si)).single().apply().get

  def findByStreetEdge(streetEdgeId: Long)(implicit session: DBSession = StreetInfo.autoSession): StreetInfo = withSQL {
    select
      .all(si)
      .from(StreetEdge as se)
      .innerJoin(StreetInfo as si).on(se.streetInfoId, si.id)
      .where.eq(se.id, streetEdgeId)
  }.map(streetInfo(si)).single().apply().get

  def create(streetInfo: StreetInfo)(implicit session: DBSession = StreetInfo.autoSession): Long = {
    withSQL {
      insert.into(StreetInfo).namedValues(
        StreetInfo.column.address -> streetInfo.address,
        StreetInfo.column.wayId -> streetInfo.wayId)
    }.updateAndReturnGeneratedKey.apply()
  }

  def deleteAll(implicit session: DBSession = StreetInfo.autoSession): Unit = withSQL {
    deleteFrom(StreetInfo)
  }.update.apply()

}

object StreetInfoRepository extends StreetInfoRepository
