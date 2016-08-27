package mapgenerator.sidewalk

import base.{ FailureReporterSupport, LazyLoggerSupport, LogicError }
import mapdomain.graph.{ Coordinate, GeoEdge, GeoVertex, GraphContainer }
import mapdomain.math.{ GVector, VectorUtils }
import mapdomain.sidewalk.SidewalkEdge
import mapdomain.sidewalk.SidewalkEdge.Side
import mapdomain.utils.EdgeUtils

case class SidewalkModule[V <: GeoVertex](implicit graph: GraphContainer[V]) extends LazyLoggerSupport with FailureReporterSupport {

  import mapdomain.utils.PointUtils._

  implicit protected val vertexIdGenerator = SidewalkVertexIDGenerator()

  def createSideWalks(distanceToStreet: Double = SidewalkModule.defaultDistanceToStreet): Set[SidewalkEdge] = {
    logger.debug(s"Create Sidewalks for all the graph")
    val builder = SideWalkBuilder()
    var verticesVisited = 0
    for (vertex ← graph.vertices if vertex.edges.nonEmpty) { // FIXME a temporary workaround: vertex.edges.nonEmpty
      verticesVisited += 1
      logger.debug(s"Visiting vertex id = ${vertex.id}, number = $verticesVisited. Vertex: $vertex")
      createSidewalkByStreetVertex(builder, vertex, distanceToStreet)
    }
    builder.build
  }

  protected def createSidewalkByStreetVertex(builder: SideWalkBuilder[V], vertex: V, distanceToStreet: Double): Unit = {

    val sortedEdges: List[GeoEdge] = GeoVertex.sortEdgesByAngle(vertex)

    // FIXME a temporary workaround in order to get edges with distinct source and destination
    val distinctEdges: List[GeoEdge] = sortedEdges.foldLeft(List.empty[GeoEdge]) { case (list, edge) =>
      if (list.exists(e => (e.vertexStart == edge.vertexStart && e.vertexEnd == edge.vertexEnd) ||
        (e.vertexStart == edge.vertexEnd && e.vertexStart == edge.vertexEnd)))
        list
      else
        list :+ edge
    }

    val groupGeoEdges: List[(Option[GeoEdge], Option[GeoEdge])] = distinctEdges.sliding(2).toList match {
      case Nil => throw LogicError("SidewalkModule.createSidewalkByStreetVertex", s"A vertex without edges: $vertex")
      case list @ (List(singleEdge)) :: xs ⇒ List((Some(singleEdge), None))
      case list ⇒
        list.map {
          case firstEdge :: secondEdge :: Nil ⇒ (Some(firstEdge), Some(secondEdge))
          case geoEdges                       ⇒ throw LogicError("SidewalkModule.createSidewalkByStreetVertex", s"wrong logic, expect a list with one or two elements, but come: $geoEdges")
        } :+ (Some(distinctEdges.last), Some(distinctEdges.head))
    }

    for (geoEdges ← groupGeoEdges) yield {
      geoEdges match {
        case (Some(firstEdge), Some(secondEdge)) ⇒
          logger.debug(s"creating sidewalk corner between 2 blocks, for edges: FirstEdge: $firstEdge SecondEdge: $secondEdge")
          val (swFirstBuilder, swSecondBuilder) = withFailureLogging(createSideWalksIntersection(distanceToStreet, vertex, firstEdge, secondEdge),
            (exc: Throwable) ⇒ logger.error(s"Failed trying to create the sidewalks for an intersection between 2 street. FirstEdge: $firstEdge SecondEdge: $secondEdge", exc))
          builder.addSideWalk(swFirstBuilder)
          builder.addSideWalk(swSecondBuilder)
        case (Some(singleEdge), None) ⇒
          val (swFirstBuilder, swSecondBuilder) = createSideWalksForSingleStreet(distanceToStreet, vertex, singleEdge)
          builder.addSideWalk(swFirstBuilder)
          builder.addSideWalk(swSecondBuilder)
        case _ ⇒ throw LogicError("SidewalkModule.createSidewalkByStreetVertex", s"wrong logic, expect a group of edges with one or two elements, but come: $geoEdges")
      }
    }
  }

  protected def createSideWalksIntersection(distance: Double, vertex: GeoVertex, firstEdge: GeoEdge, secondEdge: GeoEdge) = {

    def createIntersectedVertex(vectorFirstSidewalk: GVector, vectorSecondSidewalk: GVector): SidewalkVertexBuilder = {
      val intersectionPoint = VectorUtils.getIntersectionPoint(vectorFirstSidewalk, vectorSecondSidewalk) match {
        case Some(point) ⇒ point
        case None ⇒ throw LogicError("SidewalkModule.createSideWalksIntersection",
          s"It could not find an intersection point between the vectors. vectorFirstSidewalk: $vectorFirstSidewalk. vectorSecondSidewalk: $vectorSecondSidewalk")
      }
      logger.debug(s"Create an intersected vertex ${vertex.id}")
      SidewalkVertexBuilder(Some(Coordinate(intersectionPoint.y, intersectionPoint.x)), vertex)
    }

    def createEndVertex(endVertex: GeoVertex): SidewalkVertexBuilder = SidewalkVertexBuilder(None, endVertex)

    // get vector that represent the given edge
    val firstVector = EdgeUtils.edgeToVector(firstEdge)
    val secondVector = EdgeUtils.edgeToVector(secondEdge)

    // get parallel lines from edges
    val vectorFirstSidewalk = VectorUtils.createParallelVector(firstVector, distance, antiHourRotation = true)
    val vectorSecondSidewalk = VectorUtils.createParallelVector(secondVector, distance, antiHourRotation = false)

    // get intersected sidewalk vertex
    val intersectedVertex = createIntersectedVertex(vectorFirstSidewalk, vectorSecondSidewalk)

    // add end sidewalk vertices
    val vertexEndFirst = createEndVertex(firstEdge.retrieveOppositeVertexFor(vertex.id).get)
    val vertexEndSecond = createEndVertex(secondEdge.retrieveOppositeVertexFor(vertex.id).get)

    // get what edge is at north
    val side1: Side = SidewalkEdge.sideByEdges(firstVector.line, vectorFirstSidewalk.line)
    val side2: Side = SidewalkEdge.sideByEdges(secondVector.line, vectorSecondSidewalk.line)

    // create sidewalk builder for first and second
    val sidewalkEdgeBuilderFirst = SidewalkEdgeBuilder(intersectedVertex, vertexEndFirst, firstEdge, vectorFirstSidewalk, side1)
    val sidewalkEdgeBuilderSecond = SidewalkEdgeBuilder(intersectedVertex, vertexEndSecond, secondEdge, vectorSecondSidewalk, side2)

    logger.debug(s"Create sideWalks for a corner (${sidewalkEdgeBuilderFirst.sidewalkKey}, ${sidewalkEdgeBuilderSecond.sidewalkKey}). " +
      s"First: ${sidewalkEdgeBuilderFirst.readable}. Second: ${sidewalkEdgeBuilderSecond.readable}")

    (sidewalkEdgeBuilderFirst, sidewalkEdgeBuilderSecond)
  }

  protected def createSideWalksForSingleStreet(distance: Double, vertex: GeoVertex, edge: GeoEdge) = {

    def createRightVector(edge: GeoEdge): GVector = {
      val vertexStart = edge.retrieveVertexStart.get
      val gvector = EdgeUtils.edgeToVector(edge)
      if (gvector.source ~= vertexStart.coordinate.toPoint)
        gvector
      else
        gvector.invert
    }

    // get vector that represent the given edge with the vertexStart as its source
    val edgeVector = createRightVector(edge)

    // get parallel lines from edges
    val vectorFirstSidewalk = VectorUtils.createParallelVector(edgeVector, distance, antiHourRotation = true)
    val vectorSecondSidewalk = VectorUtils.createParallelVector(edgeVector, distance, antiHourRotation = false)

    val vertexStartFirstEdge = SidewalkVertexBuilder(Some(Coordinate.fromPoint(vectorFirstSidewalk.source)), vertex)
    val vertexStartSecondEdge = SidewalkVertexBuilder(Some(Coordinate.fromPoint(vectorSecondSidewalk.source)), vertex)

    // add end sidewalk vertices
    val vertexEndWithUnknownCoordinate: SidewalkVertexBuilder = {
      val endVertexOpt = edge.retrieveOppositeVertexFor(vertex.id)
      SidewalkVertexBuilder(None, endVertexOpt.get)
    }

    // get what edge is at north
    val side1: Side = SidewalkEdge.sideByEdges(edgeVector.line, vectorFirstSidewalk.line)
    val side2: Side = SidewalkEdge.sideByEdges(edgeVector.line, vectorSecondSidewalk.line)

    // create sidewalk builder for first and second
    val sidewalkEdgeBuilderFirst = SidewalkEdgeBuilder(vertexStartFirstEdge, vertexEndWithUnknownCoordinate, edge, vectorFirstSidewalk, side1)
    val sidewalkEdgeBuilderSecond = SidewalkEdgeBuilder(vertexStartSecondEdge, vertexEndWithUnknownCoordinate, edge, vectorSecondSidewalk, side2)

    logger.debug(s"Create sideWalks for a isolated vertex [vertex id = ${vertex.id}] (${sidewalkEdgeBuilderFirst.sidewalkKey}, ${sidewalkEdgeBuilderSecond.sidewalkKey}). First: ${sidewalkEdgeBuilderFirst.readable}. Second: ${sidewalkEdgeBuilderSecond.readable}")

    (sidewalkEdgeBuilderFirst, sidewalkEdgeBuilderSecond)
  }
}

object SidewalkModule {
  val defaultDistanceToStreet: Double = {
    val c1 = Coordinate(-34.6124922, -58.4130873)
    val c2 = Coordinate(-34.6125422, -58.4130873)
    c1.distanceToInDegrees(c2)
  }
}
