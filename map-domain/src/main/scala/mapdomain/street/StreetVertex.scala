package mapdomain.street

import mapdomain.graph._
import scalikejdbc._

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

case class UnsavedStreetVertex(override val id: Long, override val edges: List[StreetEdgeUnsaved], override val coordinate: Coordinate) extends StreetVertex(id, edges, coordinate) {

  def copy(edges: Seq[StreetEdgeUnsaved]): UnsavedStreetVertex = UnsavedStreetVertex(id, edges.toList, coordinate)
}
