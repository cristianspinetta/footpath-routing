package service

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import mapdomain.graph.{Edge => _, Vertex => _, _}
import mapdomain.repository.sidewalk.{RampRepository, SidewalkEdgeRepository}
import mapdomain.sidewalk._
import mapdomain.street.StreetEdge
import model._
import provider.{GraphSupport, PublicTransportProviderSupport}

import scala.language.existentials
import scala.util.Try

trait MapServiceSupport {
  val mapService: MapService = MapService
}

trait MapService extends GraphSupport with LazyLoggerSupport with ApiEnvConfig with PublicTransportProviderSupport {

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

  def ramps(coordinate: Coordinate, radius: Double): Try[Vector[Ramp]] = Try {
    RampRepository.findNearestRamps(coordinate, radius).toVector
  }

  def reportableElements(northEast: Coordinate, southWest: Coordinate): Try[Vector[ReportableElement]] = Try {
    val ramps = RampRepository.findRampsInRectangle(northEast, southWest)
    val sidewalks = SidewalkEdgeRepository.findSidewalksInRectangle(northEast, southWest)
    ramps.map(r => ReportableElement(r)).toVector ++ sidewalks.map(s => ReportableElement(s)).toVector
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
