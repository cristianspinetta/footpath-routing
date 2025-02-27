package service

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import mapdomain.graph.{Edge => _, Vertex => _, _}
import mapdomain.publictransport.PublicTransportCombination
import mapdomain.repository.publictransport.StopRepository
import mapdomain.repository.sidewalk.{RampRepository, SidewalkEdgeRepository}
import mapdomain.sidewalk._
import mapdomain.street.StreetEdge
import model._
import provider.{GraphSupport, PublicTransportProviderSupport, RampProviderSupport}
import scalikejdbc.DB

import scala.language.existentials
import scala.util.Try

trait MapServiceSupport {
  val mapService: MapService = MapService
}

trait MapService extends GraphSupport with LazyLoggerSupport with ApiEnvConfig with PublicTransportProviderSupport
  with RampProviderSupport {

  def edges(edgeType: EdgeType, startPosition: Coordinate, radius: Double): Try[MapContainer] = Try {
    logger.info(s"Getting edges. Type: $edgeType")
    edgeType match {
      case StreetEdgeType ⇒
        val streetEdges: List[StreetEdge] = graphs.streetDB.findNearestStreets(startPosition, radius)
        val (edges, vertices: List[Vertex]) = getEdgesAndVertices(streetEdges, graphs.street)
        MapContainer(edges, vertices)
      case SidewalkEdgeType ⇒
        val nearestEdges: List[PedestrianEdge] = graphs.sidewalkDB.findNearestSidewalks(startPosition, radius) ++:
          graphs.sidewalkDB.findNearestStreetCrossing(startPosition, radius)
        val (edges, vertices) = getEdgesAndVertices(nearestEdges, graphs.sidewalk)
        MapContainer(edges, vertices)
      case _ ⇒ MapContainer(Nil, Nil)
    }
  }

  def ramps(coordinate: Coordinate, radius: Double, associated: Boolean): Try[List[Ramp]] = Try {
    rampProvider.findNearestRamps(coordinate, radius, associated)
  }

  def reportableElements(northEast: Coordinate, southWest: Coordinate): Try[Vector[ReportableElement]] = Try {
    val ramps = RampRepository.findAssociatedRampsInRectangle(northEast, southWest)
    val sidewalks = SidewalkEdgeRepository.findSidewalksInRectangle(northEast, southWest)
    val stops = groupStops(StopRepository.findStopsInRectangle(northEast, southWest))
    ramps.map(r => ReportableElement(r)).toVector ++ sidewalks.map(s => ReportableElement(s)).toVector ++ stops.map(s => ReportableElement(s)).toVector
  }

  private def groupStops(stops: List[mapdomain.publictransport.Stop]): List[mapdomain.publictransport.Stop] = {
    logger.info(s"Abount to group ${stops.size} stops")
    def groupStopsAcc(unprocessed: List[mapdomain.publictransport.Stop], processed: List[mapdomain.publictransport.Stop]): List[mapdomain.publictransport.Stop] = {
      unprocessed match {
        case h :: Nil => h :: processed
        case h :: tail => {
          val filteredStops = unprocessed.filter(_.coordinate.distanceTo(h.coordinate) > updateStopRadius)
          groupStopsAcc(filteredStops, h :: processed)
        }
        case _ => processed
      }
    }

    stops match {
      case h :: tail => groupStopsAcc(stops, List())
      case _ => stops
    }
  }

  def publicTransportPaths(coordinate: Coordinate, radiusOpt: Option[Double], lineOpt: Option[String]): Try[List[PublicTransportPath]] = Try {
    publicTransportProvider.findStopsByRadiusAndLine(coordinate, radiusOpt, lineOpt)
        .map(_.travelInfoId)
        .distinct
        .map(publicTransportProvider.findTravelInfo)
      .map(travelInfo => {
        val coordinates: List[Coordinate] = publicTransportProvider
          .getPathBetweenStops(
            publicTransportProvider.findStop(travelInfo.firstStopId),
            publicTransportProvider.findStop(travelInfo.lastStopId))
        val stops: List[Stop] = publicTransportProvider.findStopfindByTravelInfoId(travelInfo.id) map Stop.createByDomainStop
        PublicTransportPath(travelInfo.id, travelInfo.title, coordinates, stops)
      })
  }

  def publicTransportCombinationsByRadius(coordinate: Coordinate, radius: Double): Try[List[PTCombination]] = Try {
    val combinations: List[PublicTransportCombination] = publicTransportProvider.getTPCombinationsByRadius(coordinate, radius)
    combinations
      .map(combination => (combination, publicTransportProvider.findStop(combination.fromStopId), publicTransportProvider.findStop(combination.toStopId)))
      .map { case (combination, stopFrom, stopTo) =>
        PTCombination(stopFrom.id, stopFrom.coordinate, stopFrom.travelInfoId.toString, stopTo.id, stopTo.coordinate, stopTo.travelInfoId.toString) }
  }

  private val updateStopRadius: Double = 0.01 // 10 meters

  def updateStops(stopId: Long, enabled: Boolean) = Try {
    val stop = StopRepository.find(stopId).get
    val stops = StopRepository.findByRadiusAndLine(stop.coordinate, Some(updateStopRadius))
    stops.foreach(s => StopRepository.save(s.copy(isAccessible = enabled)))
  }

  protected def getEdgesAndVertices[E <: GeoEdge with BaseEntity, G <: GraphContainer[E, V] forSome { type V <: GeoVertex[E]}](edges: List[E], graph: G) = {

    val (edgesC, vertices) = edges.foldLeft((List.empty[Edge], Set.newBuilder[Vertex])) { case ((partialEdges, partialVertices), street) =>

      val vertexFrom = graph.findVertex(street.vertexStartId).get
      val vertexTo = graph.findVertex(street.vertexEndId).get
      val edge = Edge(
        id = street.id.map(_.toString).getOrElse(""),
        from = vertexFrom.coordinate,
        to = vertexTo.coordinate,
        `type` = EdgeType.create(street))

      (edge :: partialEdges, partialVertices += Vertex.createByGeoVertex(vertexFrom) += Vertex.createByGeoVertex(vertexTo))
    }
    (edgesC, vertices.result().toList)
  }

}

object MapService extends MapService
