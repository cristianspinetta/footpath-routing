package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.{ Coordinate, GeoSearch }
import mapdomain.sidewalk.{ PedestrianEdge, Ramp, SidewalkGraphContainer, SidewalkVertex }
import model._

import scala.util.Try

trait MapModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def edges(edgeType: EdgeType, startPosition: Coordinate, radius: Double): Try[Iterable[Edge]] = Try {
    logger.info(s"Getting edges. Type: $edgeType")
    edgeType match {
      case StreetEdgeType ⇒
        graphProvider.streetGraph
          .findNearestStreets(startPosition, radius)
          .map(street ⇒
            Edge(graphProvider.streetGraph.findVertex(street.vertexStartId).get.coordinate,
              graphProvider.streetGraph.findVertex(street.vertexEndId).get.coordinate))
      case SidewalkEdgeType ⇒
        implicit val sidewalkGraph: SidewalkGraphContainer = graphProvider.sidewalkGraph
        val nearestEdges: List[PedestrianEdge] = graphProvider.sidewalkGraph.findNearestSidewalks(startPosition, radius) ++:
          graphProvider.sidewalkGraph.findNearestStreetCrossing(startPosition, radius)
        nearestEdges.map(edge ⇒ Edge(edge.from.get.coordinate, edge.to.get.coordinate))(collection.breakOut)
      //      case WayEdgeType ⇒
      //        implicit val graph: GraphContainer[StreetVertex] = graphProvider.streetGraph
      //        graphProvider.osmModule.streetWays.toList.flatMap(way ⇒ Edge.pointToEdge(Way.getPath(way)(graphProvider.streetGraph)))
      //      case WayAreaEdgeType ⇒
      //        implicit val graph: GraphContainer[StreetVertex] = graphProvider.streetGraph
      //        graphProvider.osmModule.areaWays.toList.flatMap(way ⇒ Edge.pointToEdge(Way.getPath(way)(graphProvider.streetGraph)))
      case _ ⇒ Nil
    }
  }

  def ramps(coordinate: Coordinate, radius: Double): Try[Vector[Ramp]] = Try {
    GeoSearch.findNearestByRadius(coordinate, radius, graphProvider.ramps, (ramp: Ramp) ⇒ Seq(ramp.coordinate))
  }

}

object MapModule extends MapModule
