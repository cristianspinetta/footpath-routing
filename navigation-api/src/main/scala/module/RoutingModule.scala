package module

import java.net.URL

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.{ Coordinate, Edge, GraphContainer }
import mapdomain.sidewalk.Ramp
import mapdomain.street.OsmVertex
import mapgenerator.source.osm._
import pathgenerator.core.AStar
import pathgenerator.graph._
import mapdomain.utils.GraphUtils

import scala.util.{ Failure, Try }

trait RoutingModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def routing(coordinateFrom: Coordinate, coordinateTo: Coordinate): Try[List[Coordinate]] = {
    logger.info(s"Init Search from $coordinateFrom to $coordinateTo")
    (GraphContainer.findClosestVertex(graphProvider.graph, coordinateFrom), GraphContainer.findClosestVertex(graphProvider.graph, coordinateTo)) match {
      case (Some((fromVertex, _)), Some((toVertex, _))) ⇒
        logger.info(s"Vertex From: ${fromVertex.id}. Vertex To: ${toVertex.id}")
        val aStartFactory = AStar[OsmVertex, GeoHeuristic[OsmVertex]](GeoHeuristic(fromVertex)) _
        val tryEdges: Try[List[Edge]] = aStartFactory(graphProvider.graph, fromVertex, toVertex).search
        tryEdges.map(edges ⇒ GraphUtils.edgesToIds(edges) map (vertexId ⇒ graphProvider.graph.findVertex(vertexId) match {
          case Some(vertex) ⇒ vertex.coordinate
          case None         ⇒ throw new RuntimeException(s"Vertex not found $vertexId")
        }))
      case otherResult ⇒ Failure(new RuntimeException(s"It could not get a near vertex. $otherResult"))
    }
  }

  def ramps: Try[Vector[Ramp]] = Try(graphProvider.ramps)
}

object RoutingModule extends RoutingModule
