package service

import base.LazyLoggerSupport
import base.conf.ApiEnvConfig
import mapdomain.graph.{ Edge => _, Vertex => _, _ }
import mapdomain.sidewalk._
import mapdomain.street.{ StreetEdge, StreetGraphContainer, StreetVertex }
import model._
import provider.{ GraphSupport, RampProvider }

import scala.util.Try

trait MapService extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def edges(edgeType: EdgeType, startPosition: Coordinate, radius: Double): Try[MapContainer] = Try {
    logger.info(s"Getting edges. Type: $edgeType")
    edgeType match {
      case StreetEdgeType ⇒
        val streetEdges: List[StreetEdge] = graphs.streetDB.findNearestStreets(startPosition, radius)
        val (edges, vertices: List[Vertex]) = getEdgesAndVertices[StreetEdge, StreetVertex[StreetEdge], StreetGraphContainer](streetEdges, graphs.street)
        MapContainer(edges, vertices)
      case SidewalkEdgeType ⇒
        val nearestEdges: List[PedestrianEdge] = graphs.sidewalkDB.findNearestSidewalks(startPosition, radius) ++:
          graphs.sidewalkDB.findNearestStreetCrossing(startPosition, radius)
        val (edges, vertices) = getEdgesAndVertices[PedestrianEdge, SidewalkVertex, SidewalkGraphContainer](nearestEdges, graphs.sidewalk)
        MapContainer(edges, vertices)
      case _ ⇒ MapContainer(Nil, Nil)
    }
  }

  def ramps(coordinate: Coordinate, radius: Double): Try[Vector[Ramp]] = Try {
    RampRepository.findNearestRamps(coordinate, radius).toVector
  }

  protected def getEdgesAndVertices[E <: GeoEdge { val id: Option[Long] }, V <: GeoVertex[E], G <: GraphContainer[E, V]](edges: List[E], graph: G) = {

    val (edgesC, vertices) = edges.foldLeft((List.empty[Edge], Set.newBuilder[Vertex])) { case ((partialEdges, partialVertices), street) =>

      val vertexFrom: V = graph.findVertex(street.vertexStartId).get
      val vertexTo: V = graph.findVertex(street.vertexEndId).get
      val edge = Edge(
        id = street.id.map(_.toString).getOrElse(""),
        from = vertexFrom.coordinate,
        to = vertexTo.coordinate)

      (edge :: partialEdges, partialVertices += Vertex(vertexFrom.id, vertexFrom.coordinate) += Vertex(vertexTo.id, vertexTo.coordinate))
    }
    (edgesC, vertices.result().toList)
  }

}

object MapService extends MapService
