package mapdomain.repository.publictransport

import mapdomain.publictransport.{ Stop, TravelInfo, TravelInfoUnsaved }
import scalikejdbc.{ DBSession, WrappedResultSet, _ }
import sql.SpatialSQLSupport

trait TravelInfoRepository extends SpatialSQLSupport {

  val ti = TravelInfo.syntax("ti")

  def travelInfo(ti: SyntaxProvider[TravelInfo])(rs: WrappedResultSet): TravelInfo = travelInfo(ti.resultName)(rs)

  private def travelInfo(ti: ResultName[TravelInfo])(implicit rs: WrappedResultSet): TravelInfo = {
    TravelInfo(
      id = rs.long(ti.id),
      description = rs.string(ti.description),
      firstStopId = rs.long(ti.firstStopId),
      lastStopId = rs.long(ti.lastStopId),
      branch = rs.string(ti.branch),
      name = rs.string(ti.name),
      sentido = rs.string(ti.sentido),
      `type` = rs.string(ti.`type`))
  }

  def create(travelInfoUnsaved: TravelInfoUnsaved)(implicit session: DBSession = TravelInfo.autoSession): TravelInfo = {
    val id = withSQL {
      insert.into(TravelInfo).namedValues(
        TravelInfo.column.description -> travelInfoUnsaved.description,
        TravelInfo.column.firstStopId -> travelInfoUnsaved.firstStopId,
        TravelInfo.column.lastStopId -> travelInfoUnsaved.lastStopId,
        TravelInfo.column.branch -> travelInfoUnsaved.branch,
        TravelInfo.column.name -> travelInfoUnsaved.name,
        TravelInfo.column.sentido -> travelInfoUnsaved.sentido,
        TravelInfo.column.`type` -> travelInfoUnsaved.`type`)
    }.updateAndReturnGeneratedKey.apply()

    TravelInfo.createByUnsaved(id, travelInfoUnsaved)
  }

  def find(id: Long)(implicit session: DBSession = TravelInfo.autoSession): Option[TravelInfo] = withSQL {
    select
      .all(ti)
      .from(TravelInfo as ti)
      .where.eq(ti.id, id)
  }.map(travelInfo(ti)).single().apply()

  /*def find(id: Long)(implicit session: DBSession = TravelInfo.autoSession): Option[TravelInfo] = DB readOnly { implicit session: DBSession ⇒
    sql"""
       select
        ${ti.result.*}, ${fs.result.*}, ${ls.result.*}
       from
        ${TravelInfo.as(ti)}
        left join ${Stop.as(fs)} on ${ti.firstStopId} = ${fs.id}
        left join ${Stop.as(ls)} on ${ti.lastStopId} = ${ls.id}
       where
        ${ti.id} = ${id}
    """
  .map(travelInfo(ti, fs, ls)).single().apply() }*/

  def save(travelInfo: TravelInfo)(implicit session: DBSession = TravelInfo.autoSession): TravelInfo = {
    withSQL {
      update(TravelInfo).set(
        TravelInfo.column.description -> travelInfo.description,
        TravelInfo.column.firstStopId -> travelInfo.firstStopId,
        TravelInfo.column.lastStopId -> travelInfo.lastStopId).where.eq(TravelInfo.column.id, travelInfo.id)
    }.update.apply()
    travelInfo
  }

  def deleteAll(implicit session: DBSession = TravelInfo.autoSession): Unit = withSQL {
    deleteFrom(TravelInfo)
  }.update.apply()

}

object TravelInfoRepository extends TravelInfoRepository