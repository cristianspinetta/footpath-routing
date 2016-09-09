package mapdomain.street

import mapdomain.graph._
import scalikejdbc._
import sql.SpatialSQLSupport

class StreetVertex(override val id: Long, override val edges: List[StreetEdge], override val coordinate: Coordinate) extends GeoVertex(id, edges, coordinate) {
  override def toString = s"StreetVertex($id, $edges, $coordinate)"

  def copy(coord: Coordinate): StreetVertex = new StreetVertex(id, edges, coord)

  def copy(edges: Seq[StreetEdge]): StreetVertex = new StreetVertex(id, edges.toList, coordinate)

}

object StreetVertex extends SQLSyntaxSupport[StreetVertex] {

  override val tableName = "street_vertex"

  override val columns = Seq("id", "coordinate")

  override val useSnakeCaseColumnName = false

  def apply(id: Long, edges: List[StreetEdge], coordinate: Coordinate): StreetVertex = new StreetVertex(id, edges, coordinate)

}

case class TransitStopStreetVertex(override val id: Long, override val edges: List[StreetEdge], override val coordinate: Coordinate) extends StreetVertex(id, edges, coordinate)
case class ExitVertex(override val id: Long, override val edges: List[StreetEdge], override val coordinate: Coordinate, exitName: String) extends StreetVertex(id, edges, coordinate)
case class BarrierVertex(override val id: Long, override val edges: List[StreetEdge], override val coordinate: Coordinate) extends StreetVertex(id, edges, coordinate)

trait StreetVertexRepository extends SpatialSQLSupport {

  val v = StreetVertex.syntax("v")

  def streetVertexOnly(v: SyntaxProvider[StreetVertex])(rs: WrappedResultSet): StreetVertex = {
    StreetVertex(rs.long(v.resultName.id), Nil, coordinateFromResultSet(rs, v.tableAliasName))
  }

  def create(streetVertex: StreetVertex)(implicit session: DBSession = StreetVertex.autoSession): StreetVertex = {
    withSQL {
      insert.into(StreetVertex).namedValues(
        StreetVertex.column.id -> streetVertex.id,
        StreetVertex.column.coordinate -> positionToSQL(streetVertex.coordinate))
    }.update().apply()

    streetVertex
  }

  def find(id: Long)(implicit session: DBSession = StreetVertex.autoSession): Option[StreetVertex] = withSQL {
    select
      .all(v)
      .append(selectLatitudeAndLongitude(v))
      .from(StreetVertex as v)
      .where.eq(v.id, id)
  }.map(streetVertexOnly(v)).single().apply()

  def deleteAll(implicit session: DBSession = StreetVertex.autoSession): Unit = withSQL {
    deleteFrom(StreetVertex)
  }.update.apply()

}

object StreetVertexRepository extends StreetVertexRepository
