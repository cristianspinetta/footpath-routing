package service

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import mapdomain.graph.{Edge => _, Vertex => _, _}
import mapdomain.repository.sidewalk.RampRepository
import mapdomain.sidewalk._
import mapdomain.street.StreetEdge
import model._
import provider.GraphSupport

import scala.language.existentials
import scala.util.Try

trait MapService extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

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
