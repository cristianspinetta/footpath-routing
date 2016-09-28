package mapgenerator.sidewalk

import base.LazyLoggerSupport
import mapdomain.graph.{ GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.GVector
import mapdomain.sidewalk.{ Side, SidewalkEdge, SidewalkVertex }
import mapdomain.street.StreetEdge

import scala.collection.Map
import scala.collection.concurrent.TrieMap
import scala.util.{ Failure, Success, Try }

case class SidewalkEdgeBuilderManager[E <: GeoEdge, V <: GeoVertex[E]](implicit graph: GraphContainer[E, V], idGenerator: SidewalkVertexIDGenerator) extends LazyLoggerSupport {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  private val _sidewalkOnCornerByKey = new TrieMap[String, SidewalkEdgeBuilder]

  def addSideWalk(key: String,
    from: SidewalkVertexBuilder,
    streetEdgeBelongTo: StreetEdge,
    segment: GVector,
    side: Side): SidewalkEdgeBuilder = this.synchronized {
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
    streetEdgeBelongTo: StreetEdge, segment: GVector, side: Side) extends LazyLoggerSupport {

  def build(implicit idGenerator: SidewalkVertexIDGenerator): (SidewalkEdge, SidewalkVertex, SidewalkVertex) = {
    val vertexStart: SidewalkVertex = from.build
    val vertexEnd: SidewalkVertex = to.get.build
    (SidewalkEdge(vertexStart.id, vertexEnd.id, key, side, streetEdgeBelongTo.id), vertexStart, vertexEnd)
  }

  def buildFailureTolerance(implicit idGenerator: SidewalkVertexIDGenerator): Option[(SidewalkEdge, SidewalkVertex, SidewalkVertex)] = {
    Try {
      build
    } match {
      case Success(x) ⇒ Some(x)
      case Failure(exc) ⇒
        logger.error(s"Building a Sidewalk with Failure Tolerance. Failed trying to build the following sidewalk edge: $readable. Reason: ${exc.getMessage}")
        //        exc.printStackTrace()
        None
    }
  }

  def readable: String = s"SidewalkEdgeBuilder(key = $key, from = ${this.from.readable}, to = ${to.map(_.readable)}, " +
    s"street edge belong to = (start = ${streetEdgeBelongTo.vertexStartId}, end = ${streetEdgeBelongTo.vertexEndId}), " +
    s"segment = -, side: $side)"
}
