package module

import java.net.URL

import mapgenerator.source.osm.{ OSMLoaderByXml, OSMModule }
import mapgenerator.source.osm.graph.OsmVertex
import pathgenerator.core.AStar
import pathgenerator.graph._

import scala.util.Try

trait RoutingModule {

  val osmURL: URL = getClass.getResource("/map.osm")
  val xmlParser: OSMLoaderByXml = OSMLoaderByXml(osmURL)
  val osmModule: OSMModule = OSMModule(xmlParser.loadNodes, xmlParser.loadWays)
  val graph: GraphContainer[OsmVertex] = osmModule.createGraph

  def routing(startVertexId: Int, endVertexId: Int): Try[List[Coordinate]] = {
    val startVertex: OsmVertex = graph.findVertex(startVertexId.toLong).get
    val endVertex: OsmVertex = graph.findVertex(endVertexId.toLong).get
    val aStartFactory = AStar[OsmVertex, GeoHeuristic[OsmVertex]](GeoHeuristic(startVertex)) _

    val map: Try[List[Coordinate]] = aStartFactory(graph, startVertex, endVertex).search.map(list ⇒ { // Try
      list.map(edge ⇒ { // List
        graph.findVertex(edge.vertexStart).get.coordinate
      }).::(graph.findVertex(list.last.vertexEnd).get.coordinate)
    })
    map
  }
}

object RoutingModule extends RoutingModule
