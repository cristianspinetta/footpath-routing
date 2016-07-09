package module

import java.net.URL

import mapgenerator.source.osm._
import mapgenerator.source.osm.graph.{ OsmVertex, Ramp }
import pathgenerator.core.AStar
import pathgenerator.graph._
import pathgenerator.utils.GraphUtils

import scala.util.{ Success, Try }

trait RoutingModule {

  val osmURL: URL = getClass.getResource("/map.osm")
  val rampPath2014: String = getClass.getResource("/rampas.csv").getPath
  val rampPath2011: String = getClass.getResource("/rampas_2006_2011.csv").getPath
  val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)
  val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  val graphModule: GraphModule = GraphModule(OSMModule(xmlParser))
  val graph: GraphContainer[OsmVertex] = graphModule.createGraph
  private lazy val _ramps: Vector[Ramp] = rampParser.loadRamps

  def routing(startVertexId: Long, endVertexId: Long): Try[List[Coordinate]] = {
    val startVertex: OsmVertex = graph.findVertex(startVertexId.toLong).get
    val endVertex: OsmVertex = graph.findVertex(endVertexId.toLong).get
    val aStartFactory = AStar[OsmVertex, GeoHeuristic[OsmVertex]](GeoHeuristic(startVertex)) _
    val tryEdges: Try[List[Edge]] = aStartFactory(graph, startVertex, endVertex).search
    tryEdges.map(edges ⇒ GraphUtils.edgesToIds(edges) map (vertexId ⇒ graph.findVertex(vertexId) match {
      case Some(vertex) ⇒ vertex.coordinate
      case None         ⇒ throw new RuntimeException(s"Vertex not found $vertexId")
    }))
  }

  def ramps: Try[Vector[Ramp]] = Try(_ramps)
}

object RoutingModule extends RoutingModule
