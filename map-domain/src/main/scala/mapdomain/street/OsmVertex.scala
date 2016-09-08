package mapdomain.street

import mapdomain.graph._
import scalikejdbc._
import sql.SpatialSQLSupport

class OsmVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends GeoVertex(id, edges, coordinate) {
  override def toString = s"OsmVertex($id, $edges, $coordinate)"

  def copy(coord: Coordinate): OsmVertex = new OsmVertex(id, edges, coord)

  def copy(edges: Seq[OsmStreetEdge]): OsmVertex = new OsmVertex(id, edges.toList, coordinate)

}

object OsmVertex extends SQLSyntaxSupport[OsmVertex] {

  override val tableName = "OsmVertex"

  override val columns = Seq("id", "coordinate")

  override val useSnakeCaseColumnName = false

  def apply(id: Long, edges: List[OsmStreetEdge], coordinate: Coordinate): OsmVertex = new OsmVertex(id, edges, coordinate)

}

case class TransitStopStreetVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)
case class ExitVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate, exitName: String) extends OsmVertex(id, edges, coordinate)
case class BarrierVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)

trait OsmVertexRepository extends SpatialSQLSupport {

  val v = OsmVertex.syntax("v")

  def osmVertexOnly(v: SyntaxProvider[OsmVertex])(rs: WrappedResultSet): OsmVertex = {
    OsmVertex(rs.long(v.resultName.id), Nil, coordinateFromResultSet(rs, v.tableAliasName))
  }

  def create(osmVertex: OsmVertex)(implicit session: DBSession = OsmVertex.autoSession): OsmVertex = {
    withSQL {
      insert.into(OsmVertex).namedValues(
        OsmVertex.column.id -> osmVertex.id,
        OsmVertex.column.coordinate -> positionToSQL(osmVertex.coordinate))
    }.update().apply()

    osmVertex
  }

  def find(id: Long)(implicit session: DBSession = OsmVertex.autoSession): Option[OsmVertex] = withSQL {
    select
        .all(v)
        .append(selectLatitudeAndLongitude(v))
        .from(OsmVertex as v)
        .where.eq(v.id, id)
  }.map(osmVertexOnly(v)).single().apply()

  def deleteAll(implicit session: DBSession = OsmVertex.autoSession): Unit = withSQL {
    deleteFrom(OsmVertex)
  }.update.apply()

}

object OsmVertexRepository extends OsmVertexRepository
