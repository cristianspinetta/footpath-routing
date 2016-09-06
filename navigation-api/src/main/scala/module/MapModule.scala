package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.{ Coordinate, GeoSearch, GeoVertex, GraphContainer }
import mapdomain.sidewalk.{ PedestrianEdge, Ramp, SidewalkEdge, SidewalkVertex }
import mapdomain.street.{ OsmStreetEdge, OsmVertex }
import mapgenerator.source.osm.model.Way
import model._

import scala.util.Try

trait MapModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def edges(edgeType: EdgeType, startPosition: Coordinate, radius: Double): Try[Iterable[Edge]] = Try {
    logger.info(s"Getting edges. Type: $edgeType")
    edgeType match {
      case StreetEdgeType ⇒
        val streets: List[OsmStreetEdge] = graphProvider.streets
        val nearestStreets: List[OsmStreetEdge] = GeoSearch.findNearestByRadius(startPosition,
          radius, streets, (street: OsmStreetEdge) ⇒ Seq(street.osmVertexStart.coordinate, street.osmVertexEnd.coordinate))
        nearestStreets.map(street ⇒ Edge(street.osmVertexStart.coordinate, street.osmVertexEnd.coordinate))
      case SidewalkEdgeType ⇒
        implicit val graph: GraphContainer[SidewalkVertex] = graphProvider.sidewalkGraphContainer
        val pedestrianEdges: List[PedestrianEdge] = graphProvider.sidewalks ++: graphProvider.streetCrossingEdges
        val nearestEdges: List[PedestrianEdge] = GeoSearch.findNearestByRadius(startPosition, radius, pedestrianEdges,
          (edge: PedestrianEdge) ⇒
            Seq(graph.findVertex(edge.vertexStart).get.coordinate, graph.findVertex(edge.vertexEnd).get.coordinate))
        nearestEdges.map(edge ⇒ Edge(edge.from.get.coordinate, edge.to.get.coordinate))(collection.breakOut)
      case WayEdgeType ⇒
        implicit val graph: GraphContainer[OsmVertex] = graphProvider.graph
        graphProvider.osmModule.streetWays.toList.flatMap(way ⇒ Edge.pointToEdge(Way.getPath(way)(graphProvider.graph)))
      case WayAreaEdgeType ⇒
        implicit val graph: GraphContainer[OsmVertex] = graphProvider.graph
        graphProvider.osmModule.areaWays.toList.flatMap(way ⇒ Edge.pointToEdge(Way.getPath(way)(graphProvider.graph)))
      case _ ⇒ Nil
    }
  }

  def ramps(coordinate: Coordinate, radius: Double): Try[Vector[Ramp]] = Try {
    GeoSearch.findNearestByRadius(coordinate, radius, graphProvider.ramps, (ramp: Ramp) ⇒ Seq(ramp.coordinate))
  }

}

object MapModule extends MapModule
