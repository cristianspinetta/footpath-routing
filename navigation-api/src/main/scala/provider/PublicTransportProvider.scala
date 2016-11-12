package provider

import base.{ LazyLoggerSupport, MeterSupport }
import mapdomain.graph.{ Coordinate, GeoSearch }
import mapdomain.publictransport._
import mapdomain.repository.publictransport.{ PathRepository, PublicTransportRepositorySupport, StopRepository }
import scalikejdbc.DBSession
import snapshot.publictransport.StopSnapshot
import spray.json._

import scala.annotation.tailrec
import scala.collection.Map

trait PublicTransportProviderSupport {
  // def publicTransportProvider = FakePublicTransportProvider
  def publicTransportProvider = PublicTransportProvider
}

trait PublicTransportProvider extends PublicTransportRepositorySupport with MeterSupport with LazyLoggerSupport {
  import module.Protocol._

  val stopsById: Map[Long, Stop] = StopSnapshot.get()

  def findStopsByRadiusAndLine(startPosition: Coordinate, radiusOpt: Option[Double] = None, lineOpt: Option[String] = None): List[Stop] = {
    stopRepository.findByRadiusAndLine(startPosition, radiusOpt, lineOpt)
  }

  def findTravelInfo(id: Long): TravelInfo = travelInfoRepository.find(id).get

  def findStop(id: Long): Stop = stopsById(id)

  def findStopfindByTravelInfoId(id: Long): List[Stop] = stopRepository.findByTravelInfoId(id)

  def getPathBetweenStops(stopFrom: Stop, stopTo: Stop): List[Coordinate] = withTimeLogging({

    @tailrec
    def findNextPaths(fromStopId: Long, destinationStopId: Long, accumulatedPaths: List[Path]): List[Path] = {
      if (fromStopId == destinationStopId) accumulatedPaths
      else {
        val fromStop: Stop = stopsById(fromStopId)
        // FIXME: cambiar pathId por un Option
        val newAccumulatedPaths = fromStop.pathId.flatMap(pathId ⇒ pathRepository.find(pathId)).toList ::: accumulatedPaths
        if (fromStop.nextStopId.isDefined)
          findNextPaths(fromStop.nextStopId.get, destinationStopId, newAccumulatedPaths)
        else
          accumulatedPaths
      }
    }

    val pathTail = findNextPaths(stopFrom.id, stopTo.id, List.empty)
      .reverse
      .flatMap(path ⇒ (if (path.coordinates.isEmpty) "[]" else path.coordinates).parseJson.convertTo[List[Coordinate]])

    // The first and the last coordinate on the path of each stop is missed, so here we are recover it
    stopFrom.coordinate :: pathTail ::: List(stopTo.coordinate) // FIXME change the data on the DB
  }, (time: Long) ⇒ logger.info(s"Execute Get Path Between Stops in $time ms."))

  def getTPCombinationsByRadius(startPosition: Coordinate, radius: Double): List[PublicTransportCombination] = {
    publicTransportCombinationRepository.findByRadius(startPosition, radius)
  }

  def getCombinationsByMultipleTravelInfoIds(travelInfoIds: List[Long], limit: Int = 100000, excludedRadius: List[(Coordinate, Double)] = List.empty): List[PublicTransportCombination] = withTimeLogging({
    publicTransportCombinationRepository.findByMultipleTravelInfoIds(travelInfoIds, limit, excludedRadius)
  }, (timing: Long) ⇒ logger.info(s"Search Public Transport Combinations with ${travelInfoIds.size} Travel Info as filter and retrieving a maximum of the $limit rows take $timing ms."))

  def getCombinationWalkPathByStopAndTravelInfo(fromStopId: Long, toTravelInfoId: Long): PublicTransportCombinationPath = {
    publicTransportCombinationPathRepository
      .findByStopAndTravelInfo(fromStopId, toTravelInfoId)
      .getOrElse {
        logger.error(s"Unable to find public transport combination path with from stop $fromStopId and to travel info $toTravelInfoId")
        throw new RuntimeException(s"Unable to find public transport combination path with from stop $fromStopId and to travel info $toTravelInfoId")
      }
  }

}

object PublicTransportProvider extends PublicTransportProvider

trait FakePublicTransportProvider extends PublicTransportProvider {
  import module.Protocol._

  val travelInfo = TravelInfo(id = 1, description = "Line 150", firstStopId = 1, lastStopId = 10,
    branch = "A", name = "Linea 123", sentido = "Forward", `type` = "BUS")

  val travelInfoList: List[TravelInfo] = List(travelInfo)

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

  val stops: List[Stop] = List(
    Stop(id = 1, coordinate = Coordinate(-34.618462, -58.397534), nextStopId = Some(2), previousStopId = None, sequence = 1, pathId = Some(1), travelInfoId = 1, isAccessible = true),
    Stop(id = 2, coordinate = Coordinate(-34.618714, -58.401536), nextStopId = Some(3), previousStopId = Some(1), sequence = 2, pathId = Some(2), travelInfoId = 1, isAccessible = true),
    Stop(id = 3, coordinate = Coordinate(-34.618237, -58.403574), nextStopId = Some(4), previousStopId = Some(2), sequence = 3, pathId = Some(3), travelInfoId = 1, isAccessible = true),
    Stop(id = 4, coordinate = Coordinate(-34.616162, -58.404379), nextStopId = Some(5), previousStopId = Some(3), sequence = 4, pathId = Some(4), travelInfoId = 1, isAccessible = true),
    Stop(id = 5, coordinate = Coordinate(-34.613866, -58.405151), nextStopId = Some(6), previousStopId = Some(4), sequence = 5, pathId = Some(5), travelInfoId = 1, isAccessible = true),
    Stop(id = 6, coordinate = Coordinate(-34.611579, -58.405720), nextStopId = Some(7), previousStopId = Some(5), sequence = 6, pathId = Some(6), travelInfoId = 1, isAccessible = true),
    Stop(id = 7, coordinate = Coordinate(-34.609124, -58.405935), nextStopId = Some(8), previousStopId = Some(6), sequence = 7, pathId = Some(7), travelInfoId = 1, isAccessible = true),
    Stop(id = 8, coordinate = Coordinate(-34.606934, -58.405956), nextStopId = Some(9), previousStopId = Some(7), sequence = 8, pathId = Some(8), travelInfoId = 1, isAccessible = true),
    Stop(id = 9, coordinate = Coordinate(-34.605027, -58.405505), nextStopId = Some(10), previousStopId = Some(8), sequence = 9, pathId = Some(9), travelInfoId = 1, isAccessible = true),
    Stop(id = 10, coordinate = Coordinate(-34.602899, -58.404979), nextStopId = None, previousStopId = Some(9), sequence = 10, pathId = Some(10), travelInfoId = 1, isAccessible = true))

  override def findStopsByRadiusAndLine(startPosition: Coordinate, radiusOpt: Option[Double] = None, lineOpt: Option[String] = None): List[Stop] = {
    def filterByRadius(stops: List[Stop]): List[Stop] = radiusOpt.map[List[Stop]](radius ⇒
      GeoSearch.findNearestByRadius(startPosition, radius, stops, (stop: Stop) ⇒ Seq(stop.coordinate))) getOrElse stops
    def filterByLine(stops: List[Stop]): List[Stop] = lineOpt.map(line ⇒
      stops.filter(stop ⇒ travelInfoList.find(ti ⇒ ti.id == stop.travelInfoId).exists(ti ⇒ ti.name == line))) getOrElse stops

    (filterByRadius _).andThen(filterByLine)(stops)
  }

  override def findTravelInfo(id: Long): TravelInfo = if (travelInfo.id == id) travelInfo else throw new RuntimeException(s"Unknown Travel Info. ID = $id")

  override val pathRepository: PathRepository = new PathRepository {
    override def find(id: Long)(implicit session: DBSession = Path.autoSession): Option[Path] = paths.find(_.id.get == id)
  }

  override val stopRepository: StopRepository = new StopRepository {
    override def find(id: Long)(implicit session: DBSession = Stop.autoSession): Option[Stop] = stops.find(_.id == id)
  }
}

object FakePublicTransportProvider extends FakePublicTransportProvider

