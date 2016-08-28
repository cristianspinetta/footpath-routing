package mapgenerator.sidewalk

import base.IDGeneratorLong
import mapdomain.graph.{ Coordinate, GeoVertex }
import mapdomain.sidewalk.SidewalkVertex

import scala.collection.mutable.ArrayBuffer

case class SidewalkVertexIDGenerator() extends IDGeneratorLong

case class SidewalkVertexBuilderManager() {

  val _builders = ArrayBuffer[(BuilderKey, SidewalkVertexBuilder)]()

  def create(coordinate: Option[Coordinate], streetVertexBelongTo: GeoVertex, key1: String, key2: String): SidewalkVertexBuilder = {
    _builders.find { case (BuilderKey(k1, k2), builder) ⇒ (k1 == key1 && k2 == key2) || (k1 == key2 && k2 == key1) } match {
      case Some((_, builder)) ⇒ builder
      case None ⇒
        val builder: SidewalkVertexBuilder = SidewalkVertexBuilder(coordinate, streetVertexBelongTo)
        _builders += ((BuilderKey(key1, key2), builder))
        builder
    }
  }

  def createForSingle(coordinate: Option[Coordinate], streetVertexBelongTo: GeoVertex, key1: String): SidewalkVertexBuilder = {
    create(coordinate, streetVertexBelongTo, key1, "single")
  }

  case class BuilderKey(edgeKey1: String, edgeKey2: String)

}

/**
 * Create a sidewalk vertex only once time.
 * @param coordinate: the position
 * @param streetVertexBelongTo: the street vertex that belongs to
 */
case class SidewalkVertexBuilder(coordinate: Option[Coordinate], streetVertexBelongTo: GeoVertex) {
  import utils.DoubleUtils._

  private var _sidewalkVertex: Option[SidewalkVertex] = None

  def build(implicit idGenerator: SidewalkVertexIDGenerator): SidewalkVertex = synchronized {
    _sidewalkVertex.getOrElse(_build)
  }

  def readable: String = s"SidewalkVertexBuilder(coordinate = (lng: ${coordinate.map(_.longitude.readable).getOrElse("-")}, lat: ${coordinate.map(_.latitude.readable).getOrElse("-")}), vertex belong to = ${streetVertexBelongTo.id})"

  private def _build(implicit idGenerator: SidewalkVertexIDGenerator): SidewalkVertex = {
    assert(coordinate.isDefined, s"coordinate must be define to create a Sidewalk Vertex. Element: ${this.readable}")
    val createdVertex: SidewalkVertex = SidewalkVertex(idGenerator.newID, coordinate.get, Nil, Nil, streetVertexBelongTo)
    _sidewalkVertex = Some(createdVertex)
    createdVertex
  }
}
