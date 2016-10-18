package searching

import base.{ LazyLoggerSupport, MeterSupport }
import cats.data.{ Xor, XorT }
import cats.implicits._
import mapdomain.graph._
import mapdomain.publictransport.Stop
import model._
import provider.{ GraphSupport, PublicTransportProviderSupport }
import SearchRoutingErrors._

import scala.concurrent.{ ExecutionContext, Future }

trait PublicTransportRouteSearcherSupport {
  protected val publicTransportRouteSearcher = PublicTransportRouteSearcher
}

object PublicTransportRouteSearcher extends PublicTransportRouteSearcher

sealed trait PublicTransportRouteSearcher extends WalkRouteSearcherSupport
    with GraphSupport with PublicTransportProviderSupport with LazyLoggerSupport with MeterSupport {

  import base.XorTSugars._

  def search(from: Coordinate, to: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = withTimeLoggingAsync({
    searchNearestStops(from, to).flatMap(searchPathsByDirectTransport(from, to))
  }, (time: Long) ⇒ logger.info(s"Execute Search route for Public Transport took $time ms."))

  protected def searchNearestStops(coordinateFrom: Coordinate, coordinateTo: Coordinate)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, NearestStops] = XorT {
    logger.info(s"Searching nearest stops...")
    val nearestStopsFromFut = Future(publicTransportProvider.findStopsByRadiusAndLine(coordinateFrom, Some(walkRadius)))
    val nearestStopsToFut = Future(publicTransportProvider.findStopsByRadiusAndLine(coordinateTo, Some(walkRadius)))
    for {
      nearestStopsFrom ← nearestStopsFromFut
      nearestStopsTo ← nearestStopsToFut
    } yield {
      if (nearestStopsFrom.nonEmpty && nearestStopsTo.nonEmpty)
        Xor.Right(NearestStops(nearestStopsFrom, nearestStopsTo))
      else
        Xor.Left(NoStops)
    }
  }

  protected def searchPathsByDirectTransport(from: Coordinate, to: Coordinate)(nearestStops: NearestStops)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, List[Route]] = {
    logger.info("Searching paths between stops...")

    val travelInfoIds: List[Long] = nearestStops.stopsFrom.map(s ⇒ s.travelInfoId).distinct.intersect(nearestStops.stopsTo.map(s ⇒ s.travelInfoId).distinct)

    val candidatePaths: List[PathBuilder] = travelInfoIds flatMap (travelInfoId ⇒ candidatePathByTravelInfo(from, to)(nearestStops)(travelInfoId) toList)

    candidatePaths match {
      case Nil ⇒ XorT.left[Future, SearchRoutingError, List[Route]](Future.successful(NoTransportPublicForTravel))
      case _ ⇒
        candidatePaths.traverseU(pathBuilder ⇒ {
          //      pathBuilder.build match {
          //        case XorT(route) =>
          //      }
          //      val recover: XorT[Future, SearchRoutingError, List[Route]] = pathBuilder.build.map(route => List(route)).recover { case error: SearchRoutingError => List.empty[Route] }
          //      recover
          pathBuilder.build
        })
      //    val map: List[XorT[Future, SearchRoutingError, Route]] = candidatePaths.map(pathBuilder => pathBuilder.build)
      //    val tt: XorT[Future, SearchRoutingError, List[Route]] = map.traverse()
      //    val bitravers: XorT[Future, SearchRoutingError, List[Route]] = map.bitraverse()
    }
  }

  protected def candidatePathByTravelInfo(from: Coordinate, to: Coordinate)(nearestStops: NearestStops)(travelInfoId: Long): Option[PathBuilder] = {
    if (PathBuilder.canReachDestination(nearestStops.stopsFrom, nearestStops.stopsTo))
      Some(PathBuilder(from, to)(
        travelInfoId = travelInfoId,
        stopsFrom = nearestStops.stopsFrom.filter(_.travelInfoId == travelInfoId),
        stopsTo = nearestStops.stopsTo.filter(_.travelInfoId == travelInfoId)))
    else
      None
  }

  sealed protected case class PathBuilder(from: Coordinate, to: Coordinate)(travelInfoId: Long, stopsFrom: List[Stop], stopsTo: List[Stop]) extends WalkRouteSearcherSupport {
    assert(stopsFrom.nonEmpty, s"stopsFrom must be contain at least one Stop. $stopsFrom")
    assert(stopsTo.nonEmpty, s"stopsTo must be contain at least one Stop. $stopsTo")
    assert(PathBuilder.canReachDestination(stopsFrom, stopsTo), s"stopsTo must be contain at least one Stop. $stopsTo")

    private lazy val sortedStopsFrom = stopsFrom.sortBy(stop ⇒ stop.coordinate.distanceTo(from))
    private lazy val sortedStopsTo = stopsTo.sortBy(stop ⇒ stop.coordinate.distanceTo(to))

    def build(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Route] = {

      val stopFrom = sortedStopsFrom.head
      val stopTo = sortedStopsTo.head

      val transportPublicPathFut: XorT[Future, SearchRoutingError, Path] = publicTransportPath(stopFrom, stopTo)
      val walkFromFut: XorT[Future, SearchRoutingError, Path] = this.walkRouteSearcher.search(from, stopFrom.coordinate)
      val walkToFut: XorT[Future, SearchRoutingError, Path] = this.walkRouteSearcher.search(stopTo.coordinate, to)

      for {
        walkFrom ← walkFromFut
        transportPublicPath ← transportPublicPathFut
        walkTo ← walkToFut
      } yield {
        Route(List(walkFrom, transportPublicPath, walkTo))
      } // TODO Agregar path en colectivo
    }
  }

  protected def publicTransportPath(stopFrom: Stop, stopTo: Stop)(implicit ec: ExecutionContext): XorT[Future, SearchRoutingError, Path] = XorT {
    Future[Xor[SearchRoutingError, Path]] {
      //      val travelInfo = publicTransportProvider.findTravelInfo(stopFrom.travelInfoId)
      val coordinates = publicTransportProvider.getPathBetweenStops(stopFrom, stopTo)
      Xor.Right(Path(coordinates, PathDescription(BusPath, "stop from address", "stop to address")))
    } recover {
      case exc: Throwable ⇒
        logger.error(s"An error occur trying to build the path between stops. $stopFrom, $stopTo", exc)
        Xor.Left(NoPathBetweenStops)
    }
  }

  object PathBuilder {

    def canReachDestination(stopsFrom: List[Stop], stopsTo: List[Stop]): Boolean = {
      stopsFrom.maxBy(_.sequence).sequence <= stopsTo.minBy(_.sequence).sequence
    }
  }

  sealed protected case class NearestStops(stopsFrom: List[Stop], stopsTo: List[Stop])
}