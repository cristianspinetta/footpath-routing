package searching.walk

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import cats.data.{ Xor, XorT }
import mapdomain.graph._
import mapdomain.sidewalk._
import mapdomain.utils.GraphUtils
import model._
import pathgenerator.core.AStar
import pathgenerator.graph.{ GeoGCost, GeoHeuristic }
import provider.{ GraphSupport, StreetEdgeSupport, StreetInfoSupport }
import searching.SearchRoutingErrors._
import searching.{ PedestrianIncident, RampIncidentType, SidewalkIncidentType }

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Try }

trait WalkRouteSearcherSupport {
  protected val walkRouteSearcher = WalkRouteSearcher
  protected val walkRadius: Double = WalkRouteSearcher.walkRadius
}

object WalkRouteSearcher extends WalkRouteSearcher {
  val walkRadius: Double = configuration.Routing.maximumWalkRadius
}

sealed trait WalkRouteSearcher extends GraphSupport with LazyLoggerSupport with ApiEnvConfig with StreetEdgeSupport with StreetInfoSupport {

  def search(coordinateFrom: Coordinate, coordinateTo: Coordinate, heuristicType: HeuristicType = AccessibilityHeuristicType, addStartLine: Boolean = false, addEndLine: Boolean = false)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
    Future(searchPathOnGraph(graphs.sidewalk, coordinateFrom, coordinateTo, heuristicType).get)
      .map(edges ⇒ Xor.Right(createWalkPath(edges, coordinateFrom, coordinateTo, addStartLine, addEndLine))) recover {
        case exc: Throwable ⇒
          logger.error(s"Failed trying to find a path between $coordinateFrom and $coordinateTo walking.", exc)
          Xor.Left(NoPath)
      }
  }

  protected def searchPathOnGraph(graphContainer: SidewalkGraphContainer, coordinateFrom: Coordinate,
    coordinateTo: Coordinate, heuristicType: HeuristicType): Try[List[EdgeReference[PedestrianEdge, SidewalkVertex]]] = {
    (graphContainer.findNearest(coordinateFrom), graphContainer.findNearest(coordinateTo)) match {
      case (Some(fromVertex), Some(toVertex)) ⇒
        logger.info(s"Vertex From: ${fromVertex.id}. Vertex To: ${toVertex.id}")
        val aStartFactory = heuristicType match {
          case AccessibilityHeuristicType ⇒
            AStar[PedestrianEdge, SidewalkVertex, GeoHeuristic[PedestrianEdge, SidewalkVertex], WalkGCost.type](GeoHeuristic[PedestrianEdge, SidewalkVertex](fromVertex), WalkGCost) _
          case GeoHeuristicType ⇒
            AStar[PedestrianEdge, SidewalkVertex, GeoHeuristic[PedestrianEdge, SidewalkVertex], GeoGCost[PedestrianEdge, SidewalkVertex]](GeoHeuristic[PedestrianEdge, SidewalkVertex](fromVertex), GeoGCost()) _
        }
        aStartFactory(graphContainer, fromVertex, Seq(toVertex))
          .search
      case otherResult ⇒ Failure(new RuntimeException(s"It could not get a near vertex. $otherResult"))
    }
  }

  protected def createWalkPath(edges: List[EdgeReference[PedestrianEdge, SidewalkVertex]], from: Coordinate, to: Coordinate, addStartLine: Boolean, addEndLine: Boolean): Path = {
    val incidents: List[PedestrianIncident] = extractIncidents(edges.map(_.edge))

    val vertices: List[SidewalkVertex] = GraphUtils.edgeReferencesToIds(edges) map (vertexId ⇒ graphs.sidewalk.findVertex(vertexId) match {
      case Some(vertex) ⇒ vertex
      case None         ⇒ throw new RuntimeException(s"Vertex not found $vertexId while trying to create the path from the edge list.")
    })

    val path: List[PathCoordinate] = withStopLine(getCoordinatesTailRec(vertices, addEndLine, to), addStartLine, from)

    vertices match {
      case firstVertex :: secondVertex :: xs ⇒
        val from = getIntersection(firstVertex)
        val to = getIntersection(vertices.last)
        Path(path, PathDescription(WalkPath, from, to), incidents)
      case _ ⇒
        Path(path, PathDescription(WalkPath, "-", "-"), incidents)
    }
  }

  private def getCoordinates(vertices: List[SidewalkVertex], to: Coordinate, addEndLine: Boolean): List[PathCoordinate] = vertices match {
    case Nil            ⇒ List()
    case x :: Nil       ⇒ if (addEndLine) List(PathCoordinate(x.coordinate), PathCoordinate(to)) else List(PathCoordinate(x.coordinate))
    case x :: y :: tail ⇒ PathCoordinate(x.coordinate, getAddress(x, y)) :: getCoordinates(y :: tail, to, addEndLine)
  }

  private def getCoordinatesTailRec(vertices: List[SidewalkVertex], addEndLine: Boolean, to: Coordinate): List[PathCoordinate] = {
    @tailrec
    def getCoordinatesAcc(vertices: List[SidewalkVertex], result: List[PathCoordinate]): List[PathCoordinate] =
      vertices match {
        case Nil            ⇒ result
        case x :: Nil       ⇒ withStopLine(PathCoordinate(x.coordinate) :: result, addEndLine, to)
        case x :: y :: tail ⇒ getCoordinatesAcc(y :: tail, PathCoordinate(x.coordinate, getAddress(x, y)) :: result)
      }

    getCoordinatesAcc(vertices, List()).reverse
  }

  private def withStopLine(coordinates: List[PathCoordinate], introduceLine: Boolean, coordinate: Coordinate): List[PathCoordinate] = if (introduceLine) PathCoordinate(coordinate) :: coordinates else coordinates

  private def getCoordinatesZipped(vertices: List[SidewalkVertex]) = {
    val coord = new ListBuffer[PathCoordinate]
    (vertices, vertices drop 1).zipped.foreach((from, to) ⇒ coord += PathCoordinate(from.coordinate, getAddress(from, to)))
    coord += PathCoordinate(vertices.last.coordinate)

    coord.toList
  }

  protected def extractIncidents(edges: List[PedestrianEdge]): List[PedestrianIncident] = edges flatMap {
    case SidewalkEdge(vertexStartId, vertexEndId, _, _, _, id, false) ⇒
      List(PedestrianIncident(SidewalkIncidentType,
        from = Some(graphs.sidewalk.findVertex(vertexStartId).get.coordinate),
        to = Some(graphs.sidewalk.findVertex(vertexEndId).get.coordinate)))
    case StreetCrossingEdge(vertexStartId, vertexEndId, _, _, rampStartIdOpt, rampEndIdOpt) ⇒
      def aggregateIncident(ramp: Option[Long], vertexId: Long): List[PedestrianIncident] = ramp
        .map(_ ⇒ List.empty)
        .getOrElse(List(PedestrianIncident(
          RampIncidentType,
          position = Some(graphs.sidewalk.findVertex(vertexId).get.coordinate))))
      aggregateIncident(rampStartIdOpt, vertexStartId) ::: aggregateIncident(rampEndIdOpt, vertexEndId)
    case _ ⇒ List.empty
  }

  private def getIntersection(vertex: SidewalkVertex): String = {
    vertex.sidewalkEdges.map(se ⇒ streetEdgeProvider.findById(se.streetEdgeBelongToId.get).streetInfoId).distinct match {
      case Nil ⇒ "-"
      case x :: y :: tail ⇒ {
        val builder = StringBuilder.newBuilder

        val firstAddress = streetInfoProvider.findById(x).address
        if (firstAddress.isDefined)
          builder.append(firstAddress.get)

        val secondAddress = streetInfoProvider.findById(y).address
        if (firstAddress.isDefined && secondAddress.isDefined)
          builder.append(" y ")
        if (secondAddress.isDefined)
          builder.append(secondAddress.get)

        builder.toString()
      }
      case x :: _ ⇒ streetInfoProvider.findById(x).address.getOrElse("-")
    }
  }

  private def getAddress(vertexFrom: SidewalkVertex, vertexTo: SidewalkVertex): Option[String] = {
    vertexFrom.getSidewalkEdgeFor(vertexTo.id)
      .map(_.streetEdgeBelongToId.get)
      .flatMap(streetEdgeId ⇒
        streetInfoProvider.findByStreetEdgeId(streetEdgeId).address)
  }
}
