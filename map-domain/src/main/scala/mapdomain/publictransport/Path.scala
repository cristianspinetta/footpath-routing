package mapdomain.publictransport

import scalikejdbc._

case class Path(id: Option[Long] = None, coordinates: String)

object Path extends SQLSyntaxSupport[Path] {

  override val tableName = "Path"

}

trait PathRepository {

  val p = Path.syntax("p")

  def path(p: SyntaxProvider[Path])(rs: WrappedResultSet): Path = path(p.resultName)(rs)

  private def path(p: ResultName[Path])(implicit rs: WrappedResultSet): Path = {
    new Path(
      id = Some(rs.long(p.id)),
      coordinates = rs.string(p.coordinates))
  }

  def create(coordinates: String)(implicit session: DBSession = Path.autoSession): Path = {
    val id = withSQL {
      insert.into(Path).namedValues(
        Path.column.coordinates -> coordinates)
    }.updateAndReturnGeneratedKey.apply()

    Path(Some(id), coordinates)
  }

  def find(id: Long)(implicit session: DBSession = Path.autoSession): Option[Path] = {
    withSQL { select.from(Path as p).where.eq(p.id, id) }
      .map(path(p)).single.apply()
  }

  def save(path: Path)(implicit session: DBSession = Path.autoSession): Path = {
    withSQL {
      update(Path).set(
        Path.column.coordinates -> path.coordinates)
    }.update.apply()
    path
  }

  def delete(path: Path)(implicit session: DBSession = Path.autoSession): Unit = withSQL {
    deleteFrom(Path).where.eq(Path.column.id, path.id)
  }.update.apply()

  def deleteAll(implicit session: DBSession = Path.autoSession): Unit = withSQL {
    deleteFrom(Path)
  }.update.apply()

}

object PathRepository extends PathRepository