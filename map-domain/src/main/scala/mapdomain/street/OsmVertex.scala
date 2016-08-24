package mapdomain.street

import mapdomain.graph.{Coordinate, CoordinateRepository, GeoEdge, GeoVertex}
import scalikejdbc._

class OsmVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate, coordinateId: Option[Long] = None) extends GeoVertex(id, edges, coordinate) {
  override def toString = s"OsmVertex($id, $edges, $coordinate)"

  def copy(coord: Coordinate): OsmVertex = new OsmVertex(id, edges, coord, coord.id)
}

object OsmVertex extends SQLSyntaxSupport[OsmVertex] {

  override val tableName = "OsmVertex"

  override val columns = Seq("id", "coordinateId")

  override val useSnakeCaseColumnName = false

}

trait OsmVertexRepository {

  val v = OsmVertex.syntax("v")

  private val c = CoordinateRepository.c

  private def osmVertexFromSyntaxProvider(v: SyntaxProvider[OsmVertex])(rs: WrappedResultSet): OsmVertex = osmVertexFromResultSet(v.resultName)(rs)

  private def osmVertexFromResultSet(vertex: ResultName[OsmVertex])(implicit rs: WrappedResultSet): OsmVertex = {
    new OsmVertex(
      rs.long(vertex.id),
      Nil,
      null
    )
  }

  private def osmVertexWithCoordinate(v: SyntaxProvider[OsmVertex], c: SyntaxProvider[Coordinate])(rs: WrappedResultSet): OsmVertex = {
    osmVertexFromSyntaxProvider(v)(rs).copy(CoordinateRepository.coordinateFromSyntaxProvider(c)(rs))
  }

  def create(id: Long, latitude: Long, longitude: Long)(implicit session: DBSession = OsmVertex.autoSession): OsmVertex = {
    val coordinate = CoordinateRepository.create(latitude, longitude)
    withSQL {
      insert.into(OsmVertex).namedValues(
        OsmVertex.column.id -> id,
        OsmVertex.column.coordinateId -> coordinate.id)
    }.update().apply()

    new OsmVertex(id, Nil, coordinate)
  }

  def find(id: Long)(implicit session: DBSession = OsmVertex.autoSession): OsmVertex = withSQL {
    select.
      from(OsmVertex as v)
      .leftJoin(Coordinate as c).on(v.coordinateId, c.id)
      .where.eq(v.id, id)
  }.map(osmVertexWithCoordinate(v, c)).single().apply().get

  def deleteAll(implicit session: DBSession = OsmVertex.autoSession): Unit = withSQL {
    deleteFrom(OsmVertex)
  }.update.apply()

}

object OsmVertexRepository extends OsmVertexRepository

case class TransitStopStreetVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)
case class ExitVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate, exitName: String) extends OsmVertex(id, edges, coordinate)
case class BarrierVertex(override val id: Long, override val edges: List[OsmStreetEdge], override val coordinate: Coordinate) extends OsmVertex(id, edges, coordinate)

case class OsmStreetEdge(osmVertexStart: OsmVertex, osmVertexEnd: OsmVertex, override val distance: Double, wayId: Long, vertexStartId: Option[Long] = None, vertexEndId: Option[Long] = None, id: Option[Long] = None)
  extends GeoEdge(osmVertexStart.id, osmVertexEnd.id, distance, directed = true)

object OsmStreetEdge extends SQLSyntaxSupport[OsmStreetEdge] {

  override val tableName = "OsmStreetEdge"

  override val columns = Seq("id", "distance", "wayId", "vertexStartId", "vertexEndId")

  override val useSnakeCaseColumnName = false

}

trait OsmStreetEdgeRepository {

  val e = OsmStreetEdge.syntax("e")

  private val (vs, ve) = (OsmVertex.syntax("vs"), OsmVertex.syntax("ve"))

  private def osmStreetEdgeFromSyntaxProvider(e: SyntaxProvider[OsmStreetEdge])(rs: WrappedResultSet): OsmStreetEdge = osmStreetEdgeFromResultSet(e.resultName)(rs)

  private def osmStreetEdgeFromResultSet(e: ResultName[OsmStreetEdge])(implicit rs: WrappedResultSet): OsmStreetEdge = {
    new OsmStreetEdge(OsmVertexRepository.find(rs.long(e.vertexStartId)), OsmVertexRepository.find(rs.long(e.vertexEndId)), rs.double(e.distance), rs.long(e.wayId))
  }

  def create(vertexStartId: Long, vertexEndId: Long, distance: Double, wayId: Long)(implicit session: DBSession = OsmStreetEdge.autoSession): OsmStreetEdge = {
    withSQL {
      insert.into(OsmStreetEdge).namedValues(
        OsmStreetEdge.column.vertexStartId -> vertexStartId,
        OsmStreetEdge.column.vertexEndId -> vertexEndId,
        OsmStreetEdge.column.distance -> distance,
        OsmStreetEdge.column.wayId -> wayId)
    }.update().apply()

    new OsmStreetEdge(null, null, distance, wayId)
  }

  def find(id: Long)(implicit session: DBSession = OsmStreetEdge.autoSession): OsmStreetEdge = withSQL {
    select.
      from(OsmStreetEdge as e)
      .where.eq(e.id, id)
  }.map(osmStreetEdgeFromSyntaxProvider(e)).single().apply().get

  def deleteAll(implicit session: DBSession = OsmStreetEdge.autoSession): Unit = withSQL {
    deleteFrom(OsmStreetEdge)
  }.update.apply()

}

object OsmStreetEdgeRepository extends OsmStreetEdgeRepository
