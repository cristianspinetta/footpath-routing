package module

import java.net.URL

import base.LazyLoggerSupport
import mapgenerator.source.osm._
import mapgenerator.source.osm.graph.{ OsmVertex, Ramp }
import pathgenerator.core.AStar
import pathgenerator.graph._
import pathgenerator.utils.GraphUtils

import scala.util.{ Failure, Try }

trait RoutingModule extends LazyLoggerSupport {

  val osmURL: URL = getClass.getResource("/map.osm")
  val rampPath2014: String = getClass.getResource("/rampas.csv").getPath
  val rampPath2011: String = getClass.getResource("/rampas_2006_2011.csv").getPath
  val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)
  val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  val graphModule: GraphModule = GraphModule(OSMModule(xmlParser))
  val graph: GraphContainer[OsmVertex] = graphModule.createGraph
  private lazy val _ramps: Vector[Ramp] = rampParser.loadRamps

  def routing(coordinateFrom: Coordinate, coordinateTo: Coordinate): Try[List[Coordinate]] = {
    logger.info(s"Init Search from $coordinateFrom to $coordinateTo")
    (GraphContainer.findClosestVertex(graph, coordinateFrom), GraphContainer.findClosestVertex(graph, coordinateTo)) match {
      case (Some((fromVertex, _)), Some((toVertex, _))) ⇒
        logger.info(s"Vertex From: ${fromVertex.id}. Vertex To: ${toVertex.id}")
        val aStartFactory = AStar[OsmVertex, GeoHeuristic[OsmVertex]](GeoHeuristic(fromVertex)) _
        val tryEdges: Try[List[Edge]] = aStartFactory(graph, fromVertex, toVertex).search
        tryEdges.map(edges ⇒ GraphUtils.edgesToIds(edges) map (vertexId ⇒ graph.findVertex(vertexId) match {
          case Some(vertex) ⇒ vertex.coordinate
          case None         ⇒ throw new RuntimeException(s"Vertex not found $vertexId")
        }))
      case otherResult ⇒ Failure(new RuntimeException(s"It could not get a near vertex. $otherResult"))
    }
  }

  def ramps: Try[Vector[Ramp]] = Try(_ramps)
}

object RoutingModule extends RoutingModule
