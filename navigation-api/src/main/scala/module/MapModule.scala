package module

import base.LazyLoggerSupport
import conf.ApiEnvConfig
import mapgenerator.source.osm.model.Way
import model._

import scala.util.Try

trait MapModule extends GraphSupport with LazyLoggerSupport with ApiEnvConfig {

  def streets: Try[List[Street]] = Try{
    logger.info("Getting streets.")
    graphProvider.streets.map(Street.apply)
  }
  def sidewalks: Try[Set[Sidewalk]] = Try{
    logger.info("Getting sidewalks.")
    graphProvider.sidewalks.map(Sidewalk.apply)
  }
  def edges(edgeType: EdgeType): Try[Iterable[Edge]] = Try{
    logger.info(s"Getting edges. Type: $edgeType")
    edgeType match {
      case StreetEdgeType => graphProvider.streets.map(street => Edge(street.osmVertexStart.coordinate, street.osmVertexEnd.coordinate))
      case SidewalkEdgeType =>
        val sidewalks = graphProvider.sidewalks.map(sidewalk => Edge(sidewalk.from.coordinate, sidewalk.to.coordinate))
        val streetCrossings = graphProvider.streetCrossingEdges.map(sidewalk => Edge(sidewalk.from.coordinate, sidewalk.to.coordinate))
        sidewalks.toSeq ++: streetCrossings.toSeq
      case WayEdgeType => graphProvider.osmModule.streetWays.toList.flatMap(way => Edge.pointToEdge(Way.getPath(way)(graphProvider.graph)))
      case WayAreaEdgeType => graphProvider.osmModule.areaWays.toList.flatMap(way => Edge.pointToEdge(Way.getPath(way)(graphProvider.graph)))
      case _ => Nil
    }
  }

}

object MapModule extends MapModule
