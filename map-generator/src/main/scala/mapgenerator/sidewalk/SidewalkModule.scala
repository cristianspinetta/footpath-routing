package mapgenerator.sidewalk

import base.{ FailureReporterSupport, LazyLoggerSupport, LogicError }
import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.{ GVector, VectorUtils }
import mapdomain.sidewalk.{ SidewalkEdge, SidewalkGraphContainer, SidewalkVertex, Side }
import mapdomain.utils.EdgeUtils

case class SidewalkModule[V <: GeoVertex](implicit graph: GraphContainer[V]) extends LazyLoggerSupport with FailureReporterSupport {

  import mapdomain.utils.PointUtils._
  implicit protected val vertexIdGenerator = SidewalkVertexIDGenerator()

  def createSideWalks(distanceToStreet: Double = SidewalkModule.defaultDistanceToStreet,
    failureTolerance: Boolean = false): SidewalkGraphContainer = {
    logger.info(s"Creating Sidewalks for all the graph")

    implicit val builders = Builders(StreetCrossingBuilderManager(), SidewalkVertexBuilderManager(), SidewalkEdgeBuilderManager())
    var verticesVisited = 0
    for (vertex ← graph.vertices if vertex.edges.nonEmpty) { // FIXME a temporary workaround: vertex.edges.nonEmpty
      verticesVisited += 1
      logger.debug(s"Visiting vertex id = ${vertex.id}, number = $verticesVisited. Vertex: $vertex")
      createSidewalkByStreetVertex(vertex, distanceToStreet)
    }
    val vertices: Set[SidewalkVertex] = SideWalkBuilder.build(failureTolerance)
    SidewalkGraphContainer(vertices.toList)
  }

  protected def createSidewalkByStreetVertex(vertex: V, distanceToStreet: Double)(implicit builders: Builders[V]): Unit = {

    val sortedEdges: List[GeoEdge] = GeoVertex.sortEdgesByAngle(vertex)

    // FIXME a temporary workaround in order to get edges with distinct source and destination
    val distinctEdges: List[GeoEdge] = sortedEdges.foldLeft(List.empty[GeoEdge]) {
      case (list, edge) ⇒
        if (list.exists(e ⇒ (e.vertexStartId == edge.vertexStartId && e.vertexEndId == edge.vertexEndId) ||
          (e.vertexStartId == edge.vertexEndId && e.vertexStartId == edge.vertexEndId)))
          list
        else
          list :+ edge
    }

    val groupGeoEdges: List[(Option[GeoEdge], Option[GeoEdge])] = distinctEdges.sliding(2).toList match {
      case Nil                             ⇒ throw LogicError("SidewalkModule.createSidewalkByStreetVertex", s"A vertex without edges: $vertex")
      case list @ (List(singleEdge)) :: xs ⇒ List((Some(singleEdge), None))
      case list ⇒
        list.map {
          case firstEdge :: secondEdge :: Nil ⇒ (Some(firstEdge), Some(secondEdge))
          case geoEdges ⇒ throw LogicError(
            "SidewalkModule.createSidewalkByStreetVertex", s"wrong logic, expect a list with one or two elements, but come: $geoEdges")
        } :+ (Some(distinctEdges.last), Some(distinctEdges.head))
    }

    val cornerVertexBuilders: List[SidewalkVertexBuilder] = (for (geoEdges ← groupGeoEdges) yield {
      geoEdges match {
        case (Some(firstEdge), Some(secondEdge)) ⇒
          logger.debug(s"creating sidewalk corner between 2 blocks, for edges: FirstEdge: $firstEdge SecondEdge: $secondEdge")
          val cornerVertexBuilders = withFailureLogging(createSideWalksIntersection(distanceToStreet, vertex, firstEdge, secondEdge),
            (exc: Throwable) ⇒ logger.error(
              s"Failed trying to create the sidewalks for an intersection between 2 street. FirstEdge: $firstEdge SecondEdge: $secondEdge", exc))
          cornerVertexBuilders
        case (Some(singleEdge), None) ⇒
          val cornerVertexBuilders = createSideWalksForSingleStreet(distanceToStreet, vertex, singleEdge)
          cornerVertexBuilders
        case _ ⇒ throw LogicError("SidewalkModule.createSidewalkByStreetVertex", s"wrong logic, expect a group of edges with one or two elements, but come: $geoEdges")
      }
    }) flatten

    val cornerVertexBuildersInRing = if (cornerVertexBuilders.size <= 2) cornerVertexBuilders else cornerVertexBuilders :+ cornerVertexBuilders.head
    EdgeUtils.pointToEdge[SidewalkVertexBuilder, StreetCrossingBuilder](cornerVertexBuildersInRing, (p1, p2) ⇒ builders.streetCrossingBuilderManager.create(p1, p2))
  }

  protected def createSideWalksIntersection(distance: Double, vertex: GeoVertex, firstEdge: GeoEdge,
    secondEdge: GeoEdge)(implicit builders: Builders[V]): List[SidewalkVertexBuilder] = {

    // get vector that represent the given edge
    val firstVector = EdgeUtils.edgeToVector(firstEdge)
    val secondVector = EdgeUtils.edgeToVector(secondEdge)

    // get parallel lines from edges
    val vectorFirstSidewalk = VectorUtils.createParallelVector(firstVector, distance, antiHourRotation = true)
    val vectorSecondSidewalk = VectorUtils.createParallelVector(secondVector, distance, antiHourRotation = false)

    // get what edge is at north
    val side1: Side = SidewalkEdge.sideByEdges(firstVector.line, vectorFirstSidewalk.line)
    val side2: Side = SidewalkEdge.sideByEdges(secondVector.line, vectorSecondSidewalk.line)

    val key1 = SidewalkEdge.generateKey(firstEdge, side1)
    val key2 = SidewalkEdge.generateKey(secondEdge, side2)

    def createIntersectedVertex(vectorFirstSidewalk: GVector, vectorSecondSidewalk: GVector): SidewalkVertexBuilder = {
      val intersectionPoint = VectorUtils.getIntersectionPoint(vectorFirstSidewalk, vectorSecondSidewalk) match {
        case Some(point) ⇒ point
        case None ⇒ throw LogicError("SidewalkModule.createSideWalksIntersection",
          s"It could not find an intersection point between the vectors. vectorFirstSidewalk: $vectorFirstSidewalk. vectorSecondSidewalk: $vectorSecondSidewalk")
      }
      logger.debug(s"Create an intersected vertex ${vertex.id}")
      builders.sidewalkVertexBuilderManager.create(Coordinate(intersectionPoint.y, intersectionPoint.x), vertex, key1, key2)
    }

    val intersectedVertex: SidewalkVertexBuilder = createIntersectedVertex(vectorFirstSidewalk, vectorSecondSidewalk)

    val sidewalkEdgeBuilderFirst = builders.sidewalkEdgeBuilderManager.addSideWalk(key1, intersectedVertex, firstEdge, vectorFirstSidewalk, side1)
    val sidewalkEdgeBuilderSecond = builders.sidewalkEdgeBuilderManager.addSideWalk(key2, intersectedVertex, secondEdge, vectorSecondSidewalk, side2)

    logger.debug(s"Create sideWalks for a corner (${sidewalkEdgeBuilderFirst.key}, ${sidewalkEdgeBuilderSecond.key}). " +
      s"First: ${sidewalkEdgeBuilderFirst.readable}. Second: ${sidewalkEdgeBuilderSecond.readable}")

    List(intersectedVertex)
  }

  protected def createSideWalksForSingleStreet(distance: Double, vertex: GeoVertex, edge: GeoEdge)(implicit builders: Builders[V]): List[SidewalkVertexBuilder] = {

    def createRightVector(edge: GeoEdge): GVector = {
      val vertexStart = edge.retrieveVertexStart.get
      val gvector = EdgeUtils.edgeToVector(edge)
      if (gvector.source ~= vertexStart.coordinate.toPoint)
        gvector
      else
        gvector.invert
    }

    // get vector that represent the given edge with the vertexStartId as its source
    val edgeVector = createRightVector(edge)

    // get parallel lines from edges
    val vectorFirstSidewalk = VectorUtils.createParallelVector(edgeVector, distance, antiHourRotation = true)
    val vectorSecondSidewalk = VectorUtils.createParallelVector(edgeVector, distance, antiHourRotation = false)

    // get what edge is at north
    val side1: Side = SidewalkEdge.sideByEdges(edgeVector.line, vectorFirstSidewalk.line)
    val side2: Side = SidewalkEdge.sideByEdges(edgeVector.line, vectorSecondSidewalk.line)

    val key1 = SidewalkEdge.generateKey(edge, side1)
    val key2 = SidewalkEdge.generateKey(edge, side2)

    val vertexStartFirstEdge = builders.sidewalkVertexBuilderManager.createForSingle(Coordinate.fromPoint(vectorFirstSidewalk.source), vertex, key1)
    val vertexStartSecondEdge = builders.sidewalkVertexBuilderManager.createForSingle(Coordinate.fromPoint(vectorSecondSidewalk.source), vertex, key2)

    val sidewalkEdgeBuilderFirst = builders.sidewalkEdgeBuilderManager.addSideWalk(key1, vertexStartFirstEdge, edge, vectorFirstSidewalk, side1)
    val sidewalkEdgeBuilderSecond = builders.sidewalkEdgeBuilderManager.addSideWalk(key2, vertexStartSecondEdge, edge, vectorSecondSidewalk, side2)

    logger.debug(
      s"""Create sideWalks for a isolated vertex [vertex id = ${vertex.id}] (${sidewalkEdgeBuilderFirst.key},
         |${sidewalkEdgeBuilderSecond.key}). First: ${sidewalkEdgeBuilderFirst.readable}. Second: ${sidewalkEdgeBuilderSecond.readable}""".stripMargin)

    List(vertexStartFirstEdge, vertexStartSecondEdge)
  }
}

object SidewalkModule {
  val defaultDistanceToStreet: Double = {
    val c1 = Coordinate(-34.6124922, -58.4130873)
    val c2 = Coordinate(-34.6125422, -58.4130873)
    c1.distanceToInDegrees(c2)
  }
}
