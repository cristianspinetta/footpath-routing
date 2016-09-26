package mapdomain.street

import mapdomain.graph._
import scalikejdbc._

class StreetVertex[E <: StreetEdge](override val id: Long, override val edges: List[E], override val coordinate: Coordinate) extends GeoVertex[E](id, edges, coordinate) {
  override def toString = s"StreetVertex($id, $edges, $coordinate)"

  def copy(coord: Coordinate): StreetVertex[E] = new StreetVertex[E](id, edges, coord)

  def copy(edges: Seq[E]): StreetVertex[E] = new StreetVertex(id, edges.toList, coordinate)

}

object StreetVertex extends SQLSyntaxSupport[StreetVertex[StreetEdge]] {

  type T = StreetVertex[StreetEdge]

  override val tableName = "street_vertex"

  override val columns = Seq("id", "coordinate")

  override val useSnakeCaseColumnName = false

  def apply[E <: StreetEdge](id: Long, edges: List[E], coordinate: Coordinate): StreetVertex[E] = new StreetVertex(id, edges, coordinate)

}

case class UnsavedStreetVertex(override val id: Long, override val edges: List[StreetEdgeUnsaved], override val coordinate: Coordinate) extends StreetVertex[StreetEdgeUnsaved](id, edges, coordinate) {

  override def copy(edges: Seq[StreetEdgeUnsaved]): UnsavedStreetVertex = UnsavedStreetVertex(id, edges.toList, coordinate)
}
