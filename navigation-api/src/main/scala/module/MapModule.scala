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
        graphs.street.findNearestStreets(startPosition, radius)
          .map(street ⇒
            Edge(
              id = street.id.map(_.toString).getOrElse(""),
              from = graphs.street.findVertex(street.vertexStartId).get.coordinate,
              to = graphs.street.findVertex(street.vertexEndId).get.coordinate))
      case SidewalkEdgeType ⇒
        implicit val sidewalkGraph: SidewalkGraphContainer = graphs.sidewalk
        val nearestEdges: List[PedestrianEdge] = graphs.sidewalk.findNearestSidewalks(startPosition, radius) ++:
          graphs.sidewalk.findNearestStreetCrossing(startPosition, radius)
        nearestEdges.map(edge ⇒ Edge(edge.id.map(_.toString).getOrElse(""), edge.from.get.coordinate, edge.to.get.coordinate))(collection.breakOut)
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
    GeoSearch.findNearestByRadius(coordinate, radius, RampProvider.ramps, (ramp: Ramp) ⇒ Seq(ramp.coordinate))
  }

}

object MapModule extends MapModule
