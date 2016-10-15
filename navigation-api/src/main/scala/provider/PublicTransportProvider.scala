package provider

import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph.{ Coordinate, GeoSearch }
import mapdomain.publictransport.{ Path, Stop, TravelInfo }
import mapdomain.repository.publictransport.{ PathRepository, PublicTransportRepositorySupport, StopRepository }
import scalikejdbc.DBSession
import spray.json._

import scala.annotation.tailrec

trait PublicTransportProviderSupport {
  def publicTransportProvider = FakePublicTransportProvider
}

trait PublicTransportProvider extends PublicTransportRepositorySupport with MeterSupport with LazyLoggerSupport {

  import module.Protocol._

  def findNearestStops(startPosition: Coordinate, radius: Double): List[Stop] = stopRepository.findNearestStops(startPosition, radius)

  def findTravelInfo(id: Long): TravelInfo = travelInfoRepository.find(id).get

  def getPathBetweenStops(stopFrom: Stop, stopTo: Stop): List[Coordinate] = withTimeLogging({

    @tailrec
    def findNextPaths(from: Stop, destination: Stop, accumulatedPaths: List[Path]): List[Path] = {
      if (from.id == destination.id) accumulatedPaths
      else {
        val nextStop: Stop = stopRepository.find(from.nextStopId.get).get
        findNextPaths(nextStop, destination, pathRepository.find(nextStop.pathId).get :: accumulatedPaths)
      }
    }

    findNextPaths(stopFrom, stopTo, List(pathRepository.find(stopFrom.pathId).get)).reverse.flatMap(_.coordinates.parseJson.convertTo[List[Coordinate]])
  }, (time: Long) ⇒ logger.info(s"Execute Get Path Between Stops in $time ms."))
}

object PublicTransportProvider extends PublicTransportProvider

trait FakePublicTransportProvider extends PublicTransportProvider {
  import module.Protocol._

  val travelInfo = TravelInfo(id = Some(1), description = "Line 150", firstStopId = Some(1), lastStopId = Some(10))

  val paths = List(
    Path(Some(1), coordinates = List(Coordinate(-34.618462, -58.397534), Coordinate(-34.618573, -58.399862)).toJson.compactPrint),
    Path(Some(2), coordinates = List(Coordinate(-34.618714, -58.401536), Coordinate(-34.618820, -58.403435)).toJson.compactPrint),
    Path(Some(3), coordinates = List(Coordinate(-34.618237, -58.403574), Coordinate(-34.617486, -58.403864)).toJson.compactPrint),
    Path(Some(4), coordinates = List(Coordinate(-34.616162, -58.404379)).toJson.compactPrint),
    Path(Some(5), coordinates = List(Coordinate(-34.613866, -58.405151)).toJson.compactPrint),
    Path(Some(6), coordinates = List(Coordinate(-34.611579, -58.405720)).toJson.compactPrint),
    Path(Some(7), coordinates = List(Coordinate(-34.609124, -58.405935)).toJson.compactPrint),
    Path(Some(8), coordinates = List(Coordinate(-34.606934, -58.405956)).toJson.compactPrint),
    Path(Some(9), coordinates = List(Coordinate(-34.605027, -58.405505)).toJson.compactPrint),
    Path(Some(10), coordinates = List(Coordinate(-34.602899, -58.404979)).toJson.compactPrint))

  val stops = List(
    Stop(id = 1, coordinate = Coordinate(-34.618462, -58.397534), nextStopId = Some(2), previousStopId = None, sequence = 1, pathId = 1, travelInfoId = 1, isAccessible = true),
    Stop(id = 2, coordinate = Coordinate(-34.618714, -58.401536), nextStopId = Some(3), previousStopId = Some(1), sequence = 2, pathId = 2, travelInfoId = 1, isAccessible = true),
    Stop(id = 3, coordinate = Coordinate(-34.618237, -58.403574), nextStopId = Some(4), previousStopId = Some(2), sequence = 3, pathId = 3, travelInfoId = 1, isAccessible = true),
    Stop(id = 4, coordinate = Coordinate(-34.616162, -58.404379), nextStopId = Some(5), previousStopId = Some(3), sequence = 4, pathId = 4, travelInfoId = 1, isAccessible = true),
    Stop(id = 5, coordinate = Coordinate(-34.613866, -58.405151), nextStopId = Some(6), previousStopId = Some(4), sequence = 5, pathId = 5, travelInfoId = 1, isAccessible = true),
    Stop(id = 6, coordinate = Coordinate(-34.611579, -58.405720), nextStopId = Some(7), previousStopId = Some(5), sequence = 6, pathId = 6, travelInfoId = 1, isAccessible = true),
    Stop(id = 7, coordinate = Coordinate(-34.609124, -58.405935), nextStopId = Some(8), previousStopId = Some(6), sequence = 7, pathId = 7, travelInfoId = 1, isAccessible = true),
    Stop(id = 8, coordinate = Coordinate(-34.606934, -58.405956), nextStopId = Some(9), previousStopId = Some(7), sequence = 8, pathId = 8, travelInfoId = 1, isAccessible = true),
    Stop(id = 9, coordinate = Coordinate(-34.605027, -58.405505), nextStopId = Some(10), previousStopId = Some(8), sequence = 9, pathId = 9, travelInfoId = 1, isAccessible = true),
    Stop(id = 10, coordinate = Coordinate(-34.602899, -58.404979), nextStopId = None, previousStopId = Some(9), sequence = 10, pathId = 10, travelInfoId = 1, isAccessible = true))

  override def findNearestStops(startPosition: Coordinate, radius: Double): List[Stop] = {
    GeoSearch.findNearestByRadius(startPosition, radius, stops, (stop: Stop) ⇒ Seq(stop.coordinate))
  }

  override def findTravelInfo(id: Long): TravelInfo = if (travelInfo.id.get == id) travelInfo else throw new RuntimeException(s"Unknown Travel Info. ID = $id")

  override def pathRepository: PathRepository = new PathRepository {
    override def find(id: Long)(implicit session: DBSession = Path.autoSession): Option[Path] = paths.find(_.id.get == id)
  }

  override def stopRepository: StopRepository = new StopRepository {
    override def find(id: Long)(implicit session: DBSession = Stop.autoSession): Option[Stop] = stops.find(_.id == id)
  }
}

object FakePublicTransportProvider extends FakePublicTransportProvider

