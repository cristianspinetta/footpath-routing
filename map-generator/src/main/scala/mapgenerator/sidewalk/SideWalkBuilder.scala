package mapgenerator.sidewalk

import base.{ IDGeneratorLong, LazyLoggerSupport }
import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.GVector
import mapdomain.sidewalk.SidewalkEdge.Side
import mapdomain.sidewalk.{ PedestrianEdge, SidewalkEdge, SidewalkVertex, StreetCrossingEdge }

import scala.collection.concurrent.TrieMap
import scala.collection.{ Map, mutable }
import scala.collection.mutable.ArrayBuffer
import scala.util.{ Failure, Success, Try }

case class Builders[V <: GeoVertex](sideWalkBuilder: SideWalkBuilder[V],
  streetCrossingBuilderManager: StreetCrossingBuilderManager,
  sidewalkVertexBuilderManager: SidewalkVertexBuilderManager,
  sidewalkEdgeBuilderManager: SidewalkEdgeBuilderManager[V])

case class SideWalkBuilder[V <: GeoVertex](implicit graph: GraphContainer[V], idGenerator: SidewalkVertexIDGenerator) extends LazyLoggerSupport {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  //  private val _sidewalkOnCornerByStreetVertexId = new /*TrieMap*/ mutable.ListMap[String, mutable.Set[SidewalkEdgeBuilder]] with mutable.MultiMap[String, SidewalkEdgeBuilder]
  //  private val _streetCrossingBuilders = new ArrayBuffer[StreetCrossingBuilder]()

  //  def addSideWalk(sidewalkEdgeBuilder: SidewalkEdgeBuilder): Unit = {
  //    val oldSidewalkEdgeBuilderOpt: Option[SidewalkEdgeBuilder] = getSidewalkEdgeBuilderFromMap(sidewalkEdgeBuilder)
  //
  //    oldSidewalkEdgeBuilderOpt match {
  //      case Some(oldSWEdgeBuilder) ⇒
  //        logger.debug(s"Update a Sidewalk Edge Builder: ${oldSWEdgeBuilder.readable}")
  //
  //        val newSWEdgeBuilder = oldSWEdgeBuilder.copy(
  //          from = oldSWEdgeBuilder.from.copy(
  //            coordinate =
  //              oldSWEdgeBuilder.from.coordinate orElse
  //                sidewalkEdgeBuilder.getExtremeByOwnerVertexId(oldSWEdgeBuilder.from.streetVertexBelongTo.id).flatMap(_.coordinate)),
  //          to = oldSWEdgeBuilder.to.copy(
  //            coordinate =
  //              oldSWEdgeBuilder.to.coordinate orElse
  //                sidewalkEdgeBuilder.getExtremeByOwnerVertexId(oldSWEdgeBuilder.to.streetVertexBelongTo.id).flatMap(_.coordinate)))
  //        logger.debug(s"Updated Sidewalk Edge Builder: ${newSWEdgeBuilder.readable}")
  //        _sidewalkOnCornerByStreetVertexId removeBinding (sidewalkEdgeBuilder.sidewalkKey, oldSWEdgeBuilder)
  //        _sidewalkOnCornerByStreetVertexId addBinding (sidewalkEdgeBuilder.sidewalkKey, newSWEdgeBuilder)
  //      case None ⇒
  //        logger.debug(s"Add a new Sidewalk Edge Builder: ${sidewalkEdgeBuilder.readable}")
  //        _sidewalkOnCornerByStreetVertexId addBinding (sidewalkEdgeBuilder.sidewalkKey, sidewalkEdgeBuilder)
  //    }
  //  }

  //  def addStreetCrossing(streetCrossingBuilders: List[StreetCrossingBuilder]): Unit = {
  //    println(s"adding cross: ${streetCrossingBuilders.map(_.from.streetVertexBelongTo.id).mkString(",")}")
  //    _streetCrossingBuilders ++= streetCrossingBuilders
  //  }

  //  protected def getSidewalkEdgeBuilderFromMap(sidewalkEdgeBuilder: SidewalkEdgeBuilder): Option[SidewalkEdgeBuilder] = {
  //    _sidewalkOnCornerByStreetVertexId
  //      .get(sidewalkEdgeBuilder.sidewalkKey)
  //      .flatMap(set ⇒ set.find(builder ⇒ builder.streetEdgeBelongTo.equalDirection(sidewalkEdgeBuilder.streetEdgeBelongTo)))
  //  }

  def build(implicit idGenerator: SidewalkVertexIDGenerator, builders: Builders[V]): Set[SidewalkVertex] = {

    def uniqueEdges[E <: PedestrianEdge](vertices: Set[SidewalkVertex], edges: List[E]): Map[SidewalkVertex, Set[E]] = {
      vertices.map(vertex ⇒ {
        val filteredEdges: Set[E] = edges.filter(edge ⇒ edge.vertexStart == vertex.id || edge.vertexEnd == vertex.id).toSet
        vertex -> filteredEdges
      })
        .toMap
    }

    def introduceEdges[E <: PedestrianEdge](vertices: Set[SidewalkVertex], edges: List[E],
      copyF: (SidewalkVertex, E) ⇒ SidewalkVertex): Set[SidewalkVertex] = {
      val verticesMap: Map[SidewalkVertex, Set[E]] = uniqueEdges(vertices, edges)
      val finalVertices = for {
        (vertex, edgeSet) ← verticesMap
      } yield edgeSet.foldLeft(vertex)((partialVertex, edge) ⇒ copyF(partialVertex, edge))
      finalVertices.toSet
    }

    val sidewalkEdgeBuilders: Iterable[SidewalkEdgeBuilder] = builders.sidewalkEdgeBuilderManager.sidewalkOnCornerByKey.values

    val (sidewalkEdges: Set[SidewalkEdge], verticesFromSidewalk: Set[SidewalkVertex]) = sidewalkEdgeBuilders
      .foldLeft((Set[SidewalkEdge](), Set[SidewalkVertex]())) {
        case ((swEdges, vs), builder) ⇒
          val (edge: SidewalkEdge, vertexStart: SidewalkVertex, vertexEnd: SidewalkVertex) = builder.build
          (swEdges + edge, vs + vertexStart + vertexEnd)
      }

    val (streetCrossingEdges: Set[StreetCrossingEdge], verticesFromCrossing: Set[SidewalkVertex]) = builders.streetCrossingBuilderManager.builders
      .foldLeft((Set[StreetCrossingEdge](), Set[SidewalkVertex]())) {
        case ((scEdges, vs), builder) ⇒
          val (edge: StreetCrossingEdge, vertexFrom: SidewalkVertex, vertexTo: SidewalkVertex) = builder.build
          (scEdges + edge, vs + vertexFrom + vertexTo)
      }

    val vertices: Set[SidewalkVertex] = verticesFromSidewalk ++ verticesFromCrossing

    val verticesWithSidewalks: Set[SidewalkVertex] = introduceEdges[SidewalkEdge](vertices, sidewalkEdges.toList,
      (vertex, edge) ⇒ vertex.copy(sidewalkEdges = edge :: vertex.sidewalkEdges))

    val verticesWithSidewalksAndCrossing: Set[SidewalkVertex] = introduceEdges[StreetCrossingEdge](verticesWithSidewalks,
      streetCrossingEdges.toList, (vertex, edge) ⇒ vertex.copy(streetCrossingEdges = edge :: vertex.streetCrossingEdges))

    verticesWithSidewalksAndCrossing
  }
}

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
    //    _builders.find { case (BuilderKey(k1, k2), builder) => (k1 == key1 && k2 == key2) || (k1 == key2 && k2 == key1) } match {
    //      case Some((_, builder)) => builder
    //      case None =>
    //        val builder: SidewalkVertexBuilder = SidewalkVertexBuilder(coordinate, streetVertexBelongTo)
    //        _builders += ((BuilderKey(key1, key2), builder))
    //        builder
    //    }
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

case class SidewalkEdgeBuilderManager[V <: GeoVertex](implicit graph: GraphContainer[V], idGenerator: SidewalkVertexIDGenerator) extends LazyLoggerSupport {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  private val _sidewalkOnCornerByKey = new TrieMap[String, SidewalkEdgeBuilder]
  //  private val _sidewalkOnCornerByStreetVertexId = new /*TrieMap*/ mutable.ListMap[String, mutable.Set[SidewalkEdgeBuilder]] with mutable.MultiMap[String, SidewalkEdgeBuilder]
  //  private val _streetCrossingBuilders = new ArrayBuffer[StreetCrossingBuilder]()

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
  //
  //  protected def getSidewalkEdgeBuilderFromMap(key: String, streetEdgeBelongTo: GeoEdge): Option[SidewalkEdgeBuilder] = {
  //    _sidewalkOnCornerByKey.get(key)
  //  }

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

  //  def getExtremeByOwnerVertexId(vertexId: Long): Option[SidewalkVertexBuilder] = {
  //    if (from.streetVertexBelongTo.id == vertexId) Some(from)
  //    else if (to.streetVertexBelongTo.id == vertexId) Some(to)
  //    else None
  //  }

  def readable: String = s"SidewalkEdgeBuilder(key = $key, from = ${this.from.readable}, to = ${to.map(_.readable)}, " +
    s"street edge belong to = (start = ${streetEdgeBelongTo.vertexStart}, end = ${streetEdgeBelongTo.vertexEnd}), " +
    s"segment = -, side: $side)"
}

case class StreetCrossingBuilderManager() {

  val _builders: TrieMap[(SidewalkVertexBuilder, SidewalkVertexBuilder), StreetCrossingBuilder] = TrieMap.empty

  def create(from: SidewalkVertexBuilder, to: SidewalkVertexBuilder): StreetCrossingBuilder = {
    _builders.getOrElseUpdate((from, to), StreetCrossingBuilder(from, to))
  }

  def builders: List[StreetCrossingBuilder] = _builders.values.toList
}

case class StreetCrossingBuilder(from: SidewalkVertexBuilder, to: SidewalkVertexBuilder) {

  def crossKey(fromId: Long, toId: Long): String = {
    val idPart = if (fromId > toId) s"$toId-$fromId" else s"$fromId-$toId"
    s"$idPart-cross"
  }

  def build(implicit idGenerator: SidewalkVertexIDGenerator): (StreetCrossingEdge, SidewalkVertex, SidewalkVertex) = {
    val vertexFrom: SidewalkVertex = from.build
    val vertexTo: SidewalkVertex = to.build
    (StreetCrossingEdge(vertexFrom.id, vertexTo.id, crossKey(vertexFrom.id, vertexTo.id)), vertexFrom, vertexTo)
  }
}
