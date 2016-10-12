package searching

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import cats.data.{Xor, XorT}
import mapdomain.graph._
import mapdomain.publictransport.Stop
import mapdomain.sidewalk.SidewalkVertex
import mapdomain.utils.GraphUtils
import model.{Path, PathDescription, Route, WalkPath}
import pathgenerator.core.AStar
import pathgenerator.graph.GeoHeuristic
import provider.{GraphSupport, PublicTransportProviderSupport}
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}
import scala.reflect.runtime.universe._

trait WalkRouteSearcherSupport {
  val walkRouteSearcher = WalkRouteSearcher
  val walkRadius: Double = WalkRouteSearcher.walkRadius
}

trait WalkRouteSearcher extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def search(coordinateFrom: Coordinate, coordinateTo: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
    Future(searchPathOnGraph(graphs.sidewalk, coordinateFrom, coordinateTo).get)
      .map(vertices => Xor.Right(createWalkPath(vertices))) recover {
      case exc: Throwable =>
        logger.error(s"Failed trying to find a path between $coordinateFrom and $coordinateTo walking.", exc)
        Xor.Left(WithoutPath)
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

object WalkRouteSearcher extends WalkRouteSearcher {
  val walkRadius: Double = configuration.Routing.maximumWalkRadius
}

object PublicTransportRouteSearcherSupport {
  val publicTransportRouteSearcher = PublicTransportRouteSearcher
}

trait PublicTransportRouteSearcher extends WalkRouteSearcherSupport
  with GraphSupport with PublicTransportProviderSupport with LazyLoggerSupport {

  def search(from: Coordinate, to: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = {
    searchNearestStops(from, to).flatMap(searchPathsByDirectTransport(from, to))
  }

  protected def searchNearestStops(coordinateFrom: Coordinate, coordinateTo: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, NearestStops] = XorT {
    logger.info(s"Searching nearest stops...")
    val nearestStopsFromFut = Future(publicTransportProvider.findNearestStops(coordinateFrom, walkRadius))
    val nearestStopsToFut = Future(publicTransportProvider.findNearestStops(coordinateTo, walkRadius))
    for {
      nearestStopsFrom <- nearestStopsFromFut
      nearestStopsTo <- nearestStopsToFut
    } yield {
      if (nearestStopsFrom.nonEmpty && nearestStopsTo.nonEmpty)
        Xor.Right(NearestStops(nearestStopsFrom, nearestStopsTo))
      else
        Xor.Left(WithoutStops)
    }
  }

  protected def searchPathsByDirectTransport(from: Coordinate, to: Coordinate)(nearestStops: NearestStops)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = {
    logger.info("Searching paths between stops...")
    import cats.implicits._

    val travelInfoIds: List[Long] = nearestStops.stopsFrom.map(s => s.travelInfoId.get).distinct.intersect(nearestStops.stopsTo.map(s => s.travelInfoId.get).distinct)

    val candidatePaths: List[PathBuilder] = travelInfoIds map candidatePathByTravelInfo(from, to)(nearestStops)

    val traverseU: XorT[Future, SearchRoutingError, List[Route]] = candidatePaths.traverseU(pathBuilder => {
//      pathBuilder.build match {
//        case XorT(route) =>
//      }
//      val recover: XorT[Future, SearchRoutingError, List[Route]] = pathBuilder.build.map(route => List(route)).recover { case error: SearchRoutingError => List.empty[Route] }
//      recover
      pathBuilder.build
    })

    traverseU
//    val map: List[XorT[Future, SearchRoutingError, Route]] = candidatePaths.map(pathBuilder => pathBuilder.build)
//    val tt: XorT[Future, SearchRoutingError, List[Route]] = map.traverse()
//    val bitravers: XorT[Future, SearchRoutingError, List[Route]] = map.bitraverse()
  }

  protected def candidatePathByTravelInfo(from: Coordinate, to: Coordinate)(nearestStops: NearestStops)(travelInfoId: Long): PathBuilder = {
    PathBuilder(from, to)(
      travelInfoId = travelInfoId,
      stopsFrom = nearestStops.stopsFrom.filter(_.travelInfoId == travelInfoId),
      stopsTo = nearestStops.stopsTo.filter(_.travelInfoId == travelInfoId))
  }

  sealed protected case class PathBuilder(from: Coordinate, to: Coordinate)(travelInfoId: Long, stopsFrom: List[Stop], stopsTo: List[Stop]) extends WalkRouteSearcherSupport {
    assert(stopsFrom.nonEmpty, s"stopsFrom must be contain at least one Stop. $stopsFrom")
    assert(stopsTo.nonEmpty, s"stopsTo must be contain at least one Stop. $stopsTo")

    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Route] = {

      val sortedStopsFrom = stopsFrom.sortBy(stop => stop.coordinate.distanceTo(from))
      val sortedStopsTo = stopsTo.sortBy(stop => stop.coordinate.distanceTo(to))

      val walkFromFut: XorT[Future, SearchRoutingError, Path] = this.walkRouteSearcher.search(from, sortedStopsFrom.head.coordinate)
      val walkToFut: XorT[Future, SearchRoutingError, Path] = this.walkRouteSearcher.search(sortedStopsTo.head.coordinate, to)

      for {
        walkFrom <- walkFromFut
        walkTo <- walkToFut
      } yield {
        Route(List(walkFrom, walkTo))
      } // TODO Agregar path en colectivo
    }
  }

  sealed protected case class NearestStops(stopsFrom: List[Stop], stopsTo: List[Stop])
}

object PublicTransportRouteSearcher extends PublicTransportRouteSearcher

sealed trait SearchRoutingError
case object WithoutStops extends SearchRoutingError
case object WithoutPath extends SearchRoutingError
