package searching

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import cats.data.{Xor, XorT}
import mapdomain.graph._
import mapdomain.sidewalk.SidewalkVertex
import mapdomain.utils.GraphUtils
import model.{Path, PathDescription, WalkPath}
import pathgenerator.core.AStar
import pathgenerator.graph.GeoHeuristic
import provider.GraphSupport

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.runtime.universe._
import scala.util.{Failure, Try}

import SearchRoutingErrors._

trait WalkRouteSearcherSupport {
  protected val walkRouteSearcher = WalkRouteSearcher
  protected val walkRadius: Double = WalkRouteSearcher.walkRadius
}

object WalkRouteSearcher extends WalkRouteSearcher {
  val walkRadius: Double = configuration.Routing.maximumWalkRadius
}

sealed trait WalkRouteSearcher extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def search(coordinateFrom: Coordinate, coordinateTo: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
    Future(searchPathOnGraph(graphs.sidewalk, coordinateFrom, coordinateTo).get)
      .map(vertices => Xor.Right(createWalkPath(vertices))) recover {
      case exc: Throwable =>
        logger.error(s"Failed trying to find a path between $coordinateFrom and $coordinateTo walking.", exc)
        Xor.Left(NoPath)
    }
  }

  protected def searchPathOnGraph[E <: GeoEdge, V <: GeoVertex[E] : TypeTag](graphContainer: GeoGraphContainer[E, V], coordinateFrom: Coordinate,
                                                                   coordinateTo: Coordinate): Try[List[V]] = {
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

}