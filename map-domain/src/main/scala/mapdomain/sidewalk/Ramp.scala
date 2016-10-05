package mapdomain.sidewalk

import mapdomain.graph.Coordinate
import scalikejdbc._

case class Ramp(
    coordinate: Coordinate,
    id: Option[Long] = None,
    address: String,
    var isAccessible: Boolean = true) {
}

object Ramp extends SQLSyntaxSupport[Ramp] {

  override val tableName = "ramp"

  override val columns = Seq("id", "address", "coordinate", "isAccessible")

  override val useSnakeCaseColumnName = false

}
