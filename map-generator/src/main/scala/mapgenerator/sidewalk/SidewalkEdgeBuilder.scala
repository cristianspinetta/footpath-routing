package mapgenerator.sidewalk

import base.LazyLoggerSupport
import mapdomain.graph.{ GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.GVector
import mapdomain.sidewalk.SidewalkEdge.Side
import mapdomain.sidewalk.{ SidewalkEdge, SidewalkVertex }

import scala.collection.Map
import scala.collection.concurrent.TrieMap
import scala.util.{ Failure, Success, Try }

case class SidewalkEdgeBuilderManager[V <: GeoVertex](implicit graph: GraphContainer[V], idGenerator: SidewalkVertexIDGenerator) extends LazyLoggerSupport {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  private val _sidewalkOnCornerByKey = new TrieMap[String, SidewalkEdgeBuilder]

  def addSideWalk(key: String,
    from: SidewalkVertexBuilder,
    streetEdgeBelongTo: GeoEdge,
    segment: GVector,
    side: SidewalkEdge.Side): SidewalkEdgeBuilder = {
    _sidewalkOnCornerByKey.get(key) match {
      case Some(sidewalkEdgeBuilder @ SidewalkEdgeBuilder(_, _, Some(_), _, _, _)) ⇒
        logger.debug(s"Get a created Sidewalk Edge Builder: ${sidewalkEdgeBuilder.readable}")
        sidewalkEdgeBuilder
      case Some(oldSWEdgeBuilder) ⇒
        logger.debug(s"Update a Sidewalk Edge Builder: ${oldSWEdgeBuilder.readable}")
        val newSWEdgeBuilder = oldSWEdgeBuilder.copy(to = Some(from))
        logger.debug(s"Updated Sidewalk Edge Builder: ${newSWEdgeBuilder.readable}")
        _sidewalkOnCornerByKey += (key -> newSWEdgeBuilder)
        newSWEdgeBuilder
      case None ⇒
        val newSWEdgeBuilder = SidewalkEdgeBuilder(key, from, None, streetEdgeBelongTo, segment, side)
        _sidewalkOnCornerByKey += (key -> newSWEdgeBuilder)
        logger.debug(s"Add a new Sidewalk Edge Builder: ${newSWEdgeBuilder.readable}")
        newSWEdgeBuilder
    }
  }

  def sidewalkOnCornerByKey: Map[String, SidewalkEdgeBuilder] = _sidewalkOnCornerByKey.readOnlySnapshot()
}

case class SidewalkEdgeBuilder(key: String, from: SidewalkVertexBuilder, to: Option[SidewalkVertexBuilder],
    streetEdgeBelongTo: GeoEdge, segment: GVector, side: Side) extends LazyLoggerSupport {

  def build(implicit idGenerator: SidewalkVertexIDGenerator): (SidewalkEdge, SidewalkVertex, SidewalkVertex) = {
    Try {
      val vertexStart: SidewalkVertex = from.build
      val vertexEnd: SidewalkVertex = to.get.build
      (SidewalkEdge(vertexStart.id, vertexEnd.id, key, streetEdgeBelongTo, side), vertexStart, vertexEnd)
    } match {
      case Success(x) ⇒ x
      case Failure(exc) ⇒
        logger.error(s"Failed trying to build a sidewalk edge: $readable")
        throw exc
    }
  }

  def readable: String = s"SidewalkEdgeBuilder(key = $key, from = ${this.from.readable}, to = ${to.map(_.readable)}, " +
    s"street edge belong to = (start = ${streetEdgeBelongTo.vertexStart}, end = ${streetEdgeBelongTo.vertexEnd}), " +
    s"segment = -, side: $side)"
}
