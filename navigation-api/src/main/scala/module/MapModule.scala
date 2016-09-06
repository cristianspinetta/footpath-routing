package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapdomain.graph.{ GeoVertex, GraphContainer }
import mapdomain.sidewalk.SidewalkVertex
import mapdomain.street.OsmVertex
import mapgenerator.source.osm.model.Way
import model._

import scala.util.Try

trait MapModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def streets: Try[List[Street]] = Try {
    logger.info("Getting streets.")
    graphProvider.streets.map(Street.apply)
  }
  def sidewalks: Try[List[Sidewalk]] = Try {
    logger.info("Getting sidewalks.")
    implicit val graph: GraphContainer[SidewalkVertex] = graphProvider.sidewalkGraphContainer
    graphProvider.sidewalks.map(Sidewalk.apply)
  }
  def edges(edgeType: EdgeType): Try[Iterable[Edge]] = Try {
    logger.info(s"Getting edges. Type: $edgeType")
    edgeType match {
      case StreetEdgeType ⇒ graphProvider.streets.map(street ⇒ Edge(street.osmVertexStart.coordinate, street.osmVertexEnd.coordinate))
      case SidewalkEdgeType ⇒
        implicit val graph: GraphContainer[SidewalkVertex] = graphProvider.sidewalkGraphContainer
        val sidewalks = graphProvider.sidewalks.map(sidewalk ⇒ Edge(sidewalk.from.get.coordinate, sidewalk.to.get.coordinate))
        val streetCrossings = graphProvider.streetCrossingEdges.map(sidewalk ⇒ Edge(sidewalk.from.get.coordinate, sidewalk.to.get.coordinate))
        sidewalks ++: streetCrossings
      case WayEdgeType ⇒
        implicit val graph: GraphContainer[OsmVertex] = graphProvider.graph
        graphProvider.osmModule.streetWays.toList.flatMap(way ⇒ Edge.pointToEdge(Way.getPath(way)(graphProvider.graph)))
      case WayAreaEdgeType ⇒
        implicit val graph: GraphContainer[OsmVertex] = graphProvider.graph
        graphProvider.osmModule.areaWays.toList.flatMap(way ⇒ Edge.pointToEdge(Way.getPath(way)(graphProvider.graph)))
      case _ ⇒ Nil
    }
  }

}

object MapModule extends MapModule
