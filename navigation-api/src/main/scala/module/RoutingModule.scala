package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph._
import mapdomain.sidewalk.{ Ramp, SidewalkVertex }
import mapdomain.street.StreetVertex
import pathgenerator.core.AStar
import pathgenerator.graph._
import mapdomain.utils.GraphUtils
import service.{ StreetRouting, TypeRouting }

import scala.util.{ Failure, Try }
import scala.reflect.runtime.universe._

trait RoutingModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def routing(coordinateFrom: Coordinate, coordinateTo: Coordinate, typeRouting: TypeRouting): Try[List[Coordinate]] = {
    logger.info(s"Init Search from $coordinateFrom to $coordinateTo by type routing = $typeRouting")
    val graph: GeoGraphContainer[_ <: GeoVertex] = typeRouting match {
      case StreetRouting ⇒ graphProvider.streetGraph
      case _             ⇒ graphProvider.sidewalkGraph
    }
    searchRouting(graph, coordinateFrom, coordinateTo)
  }

  protected def searchRouting[V <: GeoVertex](graphContainer: GeoGraphContainer[V], coordinateFrom: Coordinate,
    coordinateTo: Coordinate)(implicit tag: TypeTag[V]): Try[List[Coordinate]] = {
    (graphContainer.findNearest(coordinateFrom), graphContainer.findNearest(coordinateTo)) match {
      case (Some(fromVertex), Some(toVertex)) ⇒
        logger.info(s"Vertex From: ${fromVertex.id}. Vertex To: ${toVertex.id}")
        val aStartFactory = AStar[V, GeoHeuristic[V]](GeoHeuristic(fromVertex)) _
        aStartFactory(graphContainer, fromVertex, toVertex)
          .search
          .map(edges ⇒ GraphUtils.edgesToIds(edges) map (vertexId ⇒ graphContainer.findVertex(vertexId) match {
            case Some(vertex) ⇒ vertex.coordinate
            case None         ⇒ throw new RuntimeException(s"Vertex not found $vertexId")
          }))
      case otherResult ⇒ Failure(new RuntimeException(s"It could not get a near vertex. $otherResult"))
    }
  }
}

object RoutingModule extends RoutingModule
