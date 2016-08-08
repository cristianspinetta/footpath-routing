package mapgenerator.sidewalk

import base.{ IDGeneratorLong, LazyLoggerSupport, LogicError }
import mapgenerator.sidewalk.SidewalkEdge.Side
import mapgenerator.sidewalk.math._
import pathgenerator.graph.{ Coordinate, GeoEdge, GeoVertex, GraphContainer }
import scala.math._

import scala.collection.mutable

case class SidewalkModule(implicit graph: GraphContainer[GeoVertex]) extends LazyLoggerSupport {

  import utils.DoubleUtils._

  type AdjacentSidewalkCreatorFunc = (Double, GeoVertex, GeoEdge, GeoEdge) ⇒ (SidewalkEdgeBuilder, SidewalkEdgeBuilder)

  implicit protected val vertexIdGenerator = SidewalkVertexIDGenerator()

  def createSideWalks: Set[SidewalkEdge] = {
    logger.debug(s"Create Sidewalks for all the graph")
    val builder = SideWalkBuilder()
    var verticesVisited = 0
    for (vertex ← graph.vertices) {
      verticesVisited += 1
      logger.debug(s"Visiting vertex number $verticesVisited. Vertex: $vertex")
      createSidewalkByStreetVertex(builder, vertex, createSideWalksIntersection)
    }
    builder.build
  }

  protected def createSidewalkByStreetVertex(builder: SideWalkBuilder, vertex: GeoVertex,
    createAdjacentSidewalk: AdjacentSidewalkCreatorFunc): Unit = {
    val distanceToStreet: Double = 1
    val sortedEdges: List[GeoEdge] = GeoVertex.sortEdgesByAngle(vertex)

    for (geoEdges ← sortedEdges.sliding(2).toList) yield {
      geoEdges match {
        case firstEdge :: secondEdge :: Nil if VectorUtils.angleBetweenInAntiHourRotation(EdgeUtils.edgeToVector(firstEdge), EdgeUtils.edgeToVector(secondEdge)) ~<= (Pi / 2) ⇒ // FIXME debe soportar mas amplitud?
          logger.debug(s"creating sidewalk corner between 2 blocks, for edges: FirstEdge: $firstEdge SecondEdge: $secondEdge")
          val (swFirstBuilder, swSecondBuilder) = createAdjacentSidewalk(distanceToStreet, vertex, firstEdge, secondEdge)
          builder.addSideWalk(vertex.id, swFirstBuilder)
          builder.addSideWalk(vertex.id, swSecondBuilder)
        case singleEdge :: Nil ⇒
          ???
        case _ ⇒ throw LogicError("SidewalkModule.createSidewalkByStreetVertex", s"wrong logic, expect a list with one or two elements, but come: $geoEdges")
      }
    }
  }

  protected val createSideWalksIntersection: AdjacentSidewalkCreatorFunc = (distance: Double, vertex: GeoVertex, firstEdge: GeoEdge, secondEdge: GeoEdge) ⇒ {

    def createIntersectedVertex(vectorFirstSidewalk: GVector, vectorSecondSidewalk: GVector): SidewalkVertexBuilder = {
      val intersectionPoint = VectorUtils.getIntersectionPoint(vectorFirstSidewalk, vectorSecondSidewalk) match {
        case Some(point) ⇒ point
        case None ⇒ throw LogicError("SidewalkModule.createSideWalksIntersection",
          s"It could not find an intersection point between the vectors. vectorFirstSidewalk: $vectorFirstSidewalk. vectorSecondSidewalk: $vectorSecondSidewalk")
      }
      SidewalkVertexBuilder(Some(Coordinate(intersectionPoint.y, intersectionPoint.x)), vertex)
    }

    def createEndVertex(endVertex: GeoVertex): SidewalkVertexBuilder = SidewalkVertexBuilder(None, endVertex)

    // get line that represent the given edge
    val firstVector = EdgeUtils.edgeToVector(firstEdge)
    val secondVector = EdgeUtils.edgeToVector(secondEdge)

    // get parallel lines from edges
    val vectorFirstSidewalk = VectorUtils.createParallelVector(firstVector, distance, antiHourRotation = true)
    val vectorSecondSidewalk = VectorUtils.createParallelVector(secondVector, distance, antiHourRotation = false)

    // get intersected sidewalk vertex
    val intersectedVertex = createIntersectedVertex(vectorFirstSidewalk, vectorSecondSidewalk)

    // add end sidewalk vertices
    val vertexEndFirst = createEndVertex(firstEdge.retrieveVertexEnd.get)
    val vertexEndSecond = createEndVertex(secondEdge.retrieveVertexEnd.get)

    // get what edge is at north
    val side1: Side = SidewalkEdge.sideByEdges(firstVector.line, vectorFirstSidewalk.line)
    val side2: Side = SidewalkEdge.sideByEdges(secondVector.line, vectorSecondSidewalk.line)

    // create sidewalk builder for first and second
    val sidewalkEdgeBuilderFirst = SidewalkEdgeBuilder(intersectedVertex, vertexEndFirst, firstEdge, vectorFirstSidewalk, side1)
    val sidewalkEdgeBuilderSecond = SidewalkEdgeBuilder(intersectedVertex, vertexEndSecond, secondEdge, vectorSecondSidewalk, side2)

    (sidewalkEdgeBuilderFirst, sidewalkEdgeBuilderSecond)
  }
}

case class SideWalkBuilder(implicit graph: GraphContainer[GeoVertex], idGenerator: SidewalkVertexIDGenerator) {

  type SidewalkIdentity = (Long, GeoEdge, Boolean) // (street vertex id, street edge object, is at north)

  private val _sidewalkOnCornerByStreetVertexId = new /*TrieMap*/ mutable.ListMap[Long, mutable.Set[SidewalkEdgeBuilder]] with mutable.MultiMap[Long, SidewalkEdgeBuilder]

  def addSideWalk(streetVertexId: Long, sidewalkEdgeBuilder: SidewalkEdgeBuilder): Unit = {
    val oldSidewalkEdgeBuilderOpt: Option[SidewalkEdgeBuilder] = getSidewalkEdgeBuilder(streetVertexId, sidewalkEdgeBuilder.streetEdgeBelongTo)

    oldSidewalkEdgeBuilderOpt match {
      case Some(oldSWEdgeBuilder) ⇒
        val newSWEdgeBuilder = oldSWEdgeBuilder.copy(
          from = oldSWEdgeBuilder.from.copy(
            coordinate = oldSWEdgeBuilder.from.coordinate orElse sidewalkEdgeBuilder.from.coordinate),
          to = oldSWEdgeBuilder.to.copy(
            coordinate = oldSWEdgeBuilder.to.coordinate orElse sidewalkEdgeBuilder.to.coordinate))
        _sidewalkOnCornerByStreetVertexId removeBinding (streetVertexId, oldSWEdgeBuilder)
        _sidewalkOnCornerByStreetVertexId addBinding (streetVertexId, newSWEdgeBuilder)
      case None ⇒ _sidewalkOnCornerByStreetVertexId addBinding (streetVertexId, sidewalkEdgeBuilder)
    }
  }

  def getSidewalkEdgeBuilder(streetVertexId: Long, edge: GeoEdge): Option[SidewalkEdgeBuilder] = {
    _sidewalkOnCornerByStreetVertexId
      .get(streetVertexId)
      .flatMap(set ⇒ set.find(builder ⇒ builder.streetEdgeBelongTo.equalDirection(edge)))
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

case class SidewalkVertex(id: Long, coordinate: Coordinate, streetVertexBelongTo: GeoVertex)

case class SidewalkVertexBuilder(coordinate: Option[Coordinate], streetVertexBelongTo: GeoVertex) {

  def build(implicit idGenerator: SidewalkVertexIDGenerator): SidewalkVertex = {
    assert(coordinate.isDefined, "coordinate must be define to create a Sidewalk Vertex")
    SidewalkVertex(idGenerator.newID, coordinate.get, streetVertexBelongTo)
  }
}

case class SidewalkEdge(from: SidewalkVertex, to: SidewalkVertex, streetVertexBelongTo: GeoEdge, side: Side)

object SidewalkEdge {

  def sideByEdges(streetLine: Line, sidewalkLine: Line): Side = {
    if (Line.compareParallelsByAltitude(streetLine, sidewalkLine) == 1) SouthSide
    else NorthSide
  }

  trait Side
  case object NorthSide extends Side
  case object SouthSide extends Side
}

case class SidewalkEdgeBuilder(from: SidewalkVertexBuilder, to: SidewalkVertexBuilder,
    streetEdgeBelongTo: GeoEdge, segment: GVector, side: Side)(implicit idGenerator: SidewalkVertexIDGenerator) {

  def build: SidewalkEdge = SidewalkEdge(from.build, to.build, streetEdgeBelongTo, side)
}

object EdgeUtils {

  def edgeToLine(edge: GeoEdge)(implicit graph: GraphContainer[GeoVertex]): Line = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    Line.ByPairPoints(pointStart, pointEnd)
  }

  def edgeToVector(edge: GeoEdge)(implicit graph: GraphContainer[GeoVertex]): GVector = {
    val start = edge.retrieveVertexStart.get
    val end = edge.retrieveVertexEnd.get

    val pointStart = Point(start.coordinate.longitude, start.coordinate.latitude)
    val pointEnd = Point(end.coordinate.longitude, end.coordinate.latitude)

    GVector(pointStart, pointEnd)
  }
}
