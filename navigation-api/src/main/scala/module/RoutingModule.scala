package module

import java.net.URL

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.{Coordinate, Edge, GeoVertex, GraphContainer}
import mapdomain.sidewalk.{Ramp, SidewalkVertex}
import mapdomain.street.OsmVertex
import mapgenerator.source.osm._
import pathgenerator.core.AStar
import pathgenerator.graph._
import mapdomain.utils.GraphUtils
import service.{StreetRouting, TypeRouting}

import scala.util.{Failure, Try}
import scala.reflect.runtime.universe._

trait RoutingModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def routing(coordinateFrom: Coordinate, coordinateTo: Coordinate, typeRouting: TypeRouting): Try[List[Coordinate]] = {
    logger.info(s"Init Search from $coordinateFrom to $coordinateTo by type routing = $typeRouting")
    val graph: GraphContainer[_ <: GeoVertex] = typeRouting match {
      case StreetRouting => graphProvider.graph
      case _ => graphProvider.sidewalkGraphContainer
    }
    searchRouting(graph, coordinateFrom, coordinateTo)
  }

  protected def searchRouting[V <: GeoVertex](graphContainer: GraphContainer[V], coordinateFrom: Coordinate,
                                              coordinateTo: Coordinate)(implicit tag: TypeTag[V]): Try[List[Coordinate]] = {
    (GraphContainer.findClosestVertex(graphContainer, coordinateFrom), GraphContainer.findClosestVertex(graphContainer, coordinateTo)) match {
      case (Some((fromVertex, _)), Some((toVertex, _))) ⇒
        logger.info(s"Vertex From: ${fromVertex.id}. Vertex To: ${toVertex.id}")
        val aStartFactory = AStar[V, GeoHeuristic[V]](GeoHeuristic(fromVertex)) _
        aStartFactory(graphContainer, fromVertex, toVertex)
          .search
          .map(edges ⇒ GraphUtils.edgesToIds(edges) map (vertexId ⇒ graphContainer.findVertex(vertexId) match {
            case Some(vertex) ⇒ vertex.coordinate
            case None ⇒ throw new RuntimeException(s"Vertex not found $vertexId")
          }))
      case otherResult ⇒ Failure(new RuntimeException(s"It could not get a near vertex. $otherResult"))
    }
  }

  def ramps: Try[Vector[Ramp]] = Try(graphProvider.ramps)
}

object RoutingModule extends RoutingModule
