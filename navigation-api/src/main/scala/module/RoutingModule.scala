package module

import java.net.URL

import mapgenerator.source.osm._
import mapgenerator.source.osm.graph.{ OsmVertex, Ramp }
import pathgenerator.core.AStar
import pathgenerator.graph._

import scala.util.Try

trait RoutingModule {

  val osmURL: URL = getClass.getResource("/map.osm")
  val rampPath2014: String = "/home/cristian/Documents/Development/my-repositories/footpath-routing/map-generator/src/test/resources/rampas.csv"
  val rampPath2011: String = "/home/cristian/Documents/Development/my-repositories/footpath-routing/map-generator/src/test/resources/rampas_2006_2011.csv"
  val xmlParser: OSMLoaderByXml = OSMLoaderByXml(osmURL)
  val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  val osmModule: OSMModule = OSMModule(xmlParser.loadNodes, xmlParser.loadWays)
  val graph: GraphContainer[OsmVertex] = osmModule.createGraph
  private lazy val _ramps: Vector[Ramp] = rampParser.loadRamps

  def routing(startVertexId: Long, endVertexId: Long): Try[List[Coordinate]] = {
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

  def ramps: Try[Vector[Ramp]] = Try(_ramps)
}

object RoutingModule extends RoutingModule
