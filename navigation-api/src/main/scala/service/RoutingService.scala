package service

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import mapdomain.graph._
import mapdomain.sidewalk.SidewalkVertex
import mapdomain.utils.GraphUtils
import pathgenerator.core.AStar
import pathgenerator.graph._
import model._
import provider.GraphSupport

import scala.reflect.runtime.universe._
import scala.util.{Failure, Try}

trait RoutingService extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {
  val walkRadius: Double = configuration.Routing.maximumWalkRadius

  def searchRoute(coordinateFrom: Coordinate, coordinateTo: Coordinate): Try[Route] = {
    logger.info(s"Init Search from $coordinateFrom to $coordinateTo")
    if (coordinateFrom.distanceTo(coordinateTo) <= walkRadius)
      searchByWalk(coordinateFrom, coordinateTo)
    else
      searchByPublicTransport(coordinateFrom, coordinateTo)
  }

  protected def searchByWalk(coordinateFrom: Coordinate, coordinateTo: Coordinate): Try[Route] = {
    searchPathOnGraph(graphs.sidewalk, coordinateFrom, coordinateTo)
      .map(vertices => createWalkPath(vertices))
      .map(path => Route(List(path)))
  }

  protected def searchByPublicTransport(coordinateFrom: Coordinate, coordinateTo: Coordinate): Try[Route] = {
    searchPathOnGraph(graphs.sidewalk, coordinateFrom, coordinateTo)
      .map(vertices => createWalkPath(vertices))
      .map(path => Route(List(path)))
  }

  protected def createWalkPath(vertices: List[SidewalkVertex]): Path = {
    val path: List[Coordinate] = vertices.map(_.coordinate)
    vertices match {
      case firstVertex :: xs =>
        val from = "Av. Independencia 2258" // FIXME extract info from vertex
      val to = "Av. Rivadavia 1685"
        Path(path, PathDescription(WalkPath, from, to))
      case Nil =>
        Path(path, PathDescription(WalkPath, "-", "-"))
    }
  }

  protected def searchPathOnGraph[E <: GeoEdge, V <: GeoVertex[E]](graphContainer: GeoGraphContainer[E, V], coordinateFrom: Coordinate,
                                                                   coordinateTo: Coordinate)(implicit tag: TypeTag[V]): Try[List[V]] = {
    (graphContainer.findNearest(coordinateFrom), graphContainer.findNearest(coordinateTo)) match {
      case (Some(fromVertex), Some(toVertex)) ⇒
        logger.info(s"Vertex From: ${fromVertex.id}. Vertex To: ${toVertex.id}")
        val aStartFactory = AStar[E, V, GeoHeuristic[E, V]](GeoHeuristic(fromVertex)) _
        aStartFactory(graphContainer, fromVertex, Seq(toVertex))
          .search
          .map(edges ⇒
            GraphUtils.edgesToIds(edges) map (vertexId ⇒ graphContainer.findVertex(vertexId) match {
              case Some(vertex) ⇒ vertex
              case None         ⇒ throw new RuntimeException(s"Vertex not found $vertexId while trying to create the path from the edge list.")
            }))
      case otherResult ⇒ Failure(new RuntimeException(s"It could not get a near vertex. $otherResult"))
    }
  }
}

object RoutingService extends RoutingService
