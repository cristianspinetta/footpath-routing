package mapdomain.publictransport

import scalikejdbc._

case class Path(id: Option[Long] = None, coordinates: String)

object Path extends SQLSyntaxSupport[Path] {

  override val tableName = "path"

}

