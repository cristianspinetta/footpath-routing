package mapdomain.street

import mapdomain.graph._
import scalikejdbc._

sealed trait StreetVertex[E <: StreetEdge] extends GeoVertex[E] {

  def copy(coord: Coordinate): StreetVertex[E] = StreetVertex[E](id, edges, coord)

  def copy(edges: Seq[E]): StreetVertex[E] = StreetVertex(id, edges.toList, coordinate)

  override def toString = s"StreetVertex($id, $edges, $coordinate)"
}

object StreetVertex extends SQLSyntaxSupport[StreetVertex[StreetEdge]] {

  override val tableName = "street_vertex"
  override val columns = Seq("id", "coordinate")
  override val useSnakeCaseColumnName = false

  def apply[E <: StreetEdge](id: Long, edges: List[E], coordinate: Coordinate): StreetVertex[E] = StreetVertexImpl(id, edges, coordinate)
}

case class StreetVertexImpl[E <: StreetEdge](override val id: Long, override val edges: List[E], override val coordinate: Coordinate) extends StreetVertex[E]

case class UnsavedStreetVertex(override val id: Long, override val edges: List[StreetEdgeUnsaved], override val coordinate: Coordinate) extends StreetVertex[StreetEdgeUnsaved] {
  override def copy(edges: Seq[StreetEdgeUnsaved]): UnsavedStreetVertex = UnsavedStreetVertex(id, edges.toList, coordinate)
}
