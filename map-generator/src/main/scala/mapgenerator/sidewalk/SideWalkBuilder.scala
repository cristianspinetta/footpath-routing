package mapgenerator.sidewalk

import base.{ IDGeneratorLong, LazyLoggerSupport }
import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.GVector
import mapdomain.sidewalk.SidewalkEdge.Side
import mapdomain.sidewalk.{ SidewalkEdge, SidewalkVertex }

import scala.collection.mutable
import scala.util.{ Failure, Success, Try }

case class SideWalkBuilder[V <: GeoVertex](implicit graph: GraphContainer[V], idGenerator: SidewalkVertexIDGenerator) extends LazyLoggerSupport {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  private val _sidewalkOnCornerByStreetVertexId = new /*TrieMap*/ mutable.ListMap[String, mutable.Set[SidewalkEdgeBuilder]] with mutable.MultiMap[String, SidewalkEdgeBuilder]

  def addSideWalk(sidewalkEdgeBuilder: SidewalkEdgeBuilder): Unit = {
    val oldSidewalkEdgeBuilderOpt: Option[SidewalkEdgeBuilder] = getSidewalkEdgeBuilderFromMap(sidewalkEdgeBuilder)

    oldSidewalkEdgeBuilderOpt match {
      case Some(oldSWEdgeBuilder) ⇒
        logger.debug(s"Update a Sidewalk Edge Builder: ${oldSWEdgeBuilder.readable}")

        val newSWEdgeBuilder = oldSWEdgeBuilder.copy(
          from = oldSWEdgeBuilder.from.copy(
            coordinate =
              oldSWEdgeBuilder.from.coordinate orElse
                sidewalkEdgeBuilder.getExtremeByOwnerVertexId(oldSWEdgeBuilder.from.streetVertexBelongTo.id).flatMap(_.coordinate)),
          to = oldSWEdgeBuilder.to.copy(
            coordinate =
              oldSWEdgeBuilder.to.coordinate orElse
                sidewalkEdgeBuilder.getExtremeByOwnerVertexId(oldSWEdgeBuilder.to.streetVertexBelongTo.id).flatMap(_.coordinate)))
        logger.debug(s"Updated Sidewalk Edge Builder: ${newSWEdgeBuilder.readable}")
        _sidewalkOnCornerByStreetVertexId removeBinding (sidewalkEdgeBuilder.sidewalkKey, oldSWEdgeBuilder)
        _sidewalkOnCornerByStreetVertexId addBinding (sidewalkEdgeBuilder.sidewalkKey, newSWEdgeBuilder)
      case None ⇒
        logger.debug(s"Add a new Sidewalk Edge Builder: ${sidewalkEdgeBuilder.readable}")
        _sidewalkOnCornerByStreetVertexId addBinding (sidewalkEdgeBuilder.sidewalkKey, sidewalkEdgeBuilder)
    }
  }

  protected def getSidewalkEdgeBuilderFromMap(sidewalkEdgeBuilder: SidewalkEdgeBuilder): Option[SidewalkEdgeBuilder] = {
    _sidewalkOnCornerByStreetVertexId
      .get(sidewalkEdgeBuilder.sidewalkKey)
      .flatMap(set ⇒ set.find(builder ⇒ builder.streetEdgeBelongTo.equalDirection(sidewalkEdgeBuilder.streetEdgeBelongTo)))
  }

  def build: Set[SidewalkEdge] = {
    val subBuilders: Iterable[SidewalkEdgeBuilder] = _sidewalkOnCornerByStreetVertexId.values.flatten
    val sidewalkEdges = Set[SidewalkEdge]()
    subBuilders.foldLeft(sidewalkEdges) {
      case (swEdges, builder) ⇒
        swEdges + builder.build
    }
  }
}

case class SidewalkVertexIDGenerator() extends IDGeneratorLong

case class SidewalkVertexBuilder(coordinate: Option[Coordinate], streetVertexBelongTo: GeoVertex) {
  import utils.DoubleUtils._

  def build(implicit idGenerator: SidewalkVertexIDGenerator): SidewalkVertex = {
    assert(coordinate.isDefined, s"coordinate must be define to create a Sidewalk Vertex. Element: ${this.readable}")
    SidewalkVertex(idGenerator.newID, coordinate.get, streetVertexBelongTo)
  }

  def readable: String = s"SidewalkVertexBuilder(coordinate = (lng: ${coordinate.map(_.longitude.readable).getOrElse("-")}, lat: ${coordinate.map(_.latitude.readable).getOrElse("-")}), vertex belong to = ${streetVertexBelongTo.id})"
}

case class SidewalkEdgeBuilder(from: SidewalkVertexBuilder, to: SidewalkVertexBuilder,
    streetEdgeBelongTo: GeoEdge, segment: GVector, side: Side)(implicit idGenerator: SidewalkVertexIDGenerator) extends LazyLoggerSupport {

  val sidewalkKey: String = {
    val vertexFromId: Long = from.streetVertexBelongTo.id
    val vertexToId: Long = to.streetVertexBelongTo.id
    val idPart = if (vertexFromId > vertexToId) s"$vertexToId-$vertexFromId" else s"$vertexFromId-$vertexToId"
    s"$idPart-$side"
  }

  def build: SidewalkEdge = Try(SidewalkEdge(from.build, to.build, streetEdgeBelongTo, side)) match {
    case Success(x) ⇒ x
    case Failure(exc) ⇒
      logger.error(s"Failed trying to build a sidewalk edge: $readable")
      throw exc
  }

  def getExtremeByOwnerVertexId(vertexId: Long): Option[SidewalkVertexBuilder] = {
    if (from.streetVertexBelongTo.id == vertexId) Some(from)
    else if (to.streetVertexBelongTo.id == vertexId) Some(to)
    else None
  }

  def readable: String = s"SidewalkEdgeBuilder(key = $sidewalkKey, from = ${this.from.readable}, to = ${to.readable}, " +
    s"street edge belong to = (start = ${streetEdgeBelongTo.vertexStart}, end = ${streetEdgeBelongTo.vertexEnd}), " +
    s"segment = -, side: $side)"
}
