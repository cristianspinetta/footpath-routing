package mapgenerator

import java.net.URL

import base.LazyLoggerSupport
import mapdomain.graph.GraphContainer
import mapdomain.sidewalk.{ EagerSidewalkGraphContainer, Ramp, SidewalkRepositorySupport }
import mapdomain.street.{ EagerStreetGraphContainer, StreetEdge, StreetRepositorySupport, StreetVertex }
import mapgenerator.sidewalk.SidewalkModule
import mapgenerator.source.features.{ RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV }
import mapgenerator.source.osm.{ OSMModule, OSMReaderByXml }
import mapgenerator.street.StreetGraphModule
import scalikejdbc.DB

import scala.util.Try

object MapGenerator extends LazyLoggerSupport with StreetRepositorySupport with SidewalkRepositorySupport {

  def createStreets(osmURL: URL): GraphContainer[StreetVertex] = {
    // FIXME Check that streets doen't exist.
    val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)
    val osmModule: OSMModule = OSMModule(xmlParser)
    val streetGraphModule: StreetGraphModule = StreetGraphModule(osmModule)
    streetGraphModule.createGraph.purgeStreets
  }

  def saveStreets(streetGraph: EagerStreetGraphContainer): Try[_] = Try {
    DB localTx { implicit session ⇒
      val savedVertices = streetVertexRepository createInBulk streetGraph.vertices
      for {
        vertex ← savedVertices
        edge ← vertex.edges
      } {
        streetEdgeRepository.create(edge)
      }
    }
  }

  def createSidewalks(streetGraph: EagerStreetGraphContainer): EagerSidewalkGraphContainer = {
    SidewalkModule()(streetGraph).createSideWalks(failureTolerance = true).purgeSidewalks
  }

  def saveSidewalks(sidewalkGraph: EagerSidewalkGraphContainer): Try[_] = Try {
    DB localTx { implicit session ⇒
      sidewalkGraph.vertices foreach sidewalkVertexRepository.create
      sidewalkGraph.sidewalkEdges foreach sidewalkEdgeRepository.create
      sidewalkGraph.streetCrossingEdges foreach streetCrossingEdgeRepository.create
    }
  }

  def createRamps(rampPath2014: String, rampPath2011: String): Vector[Ramp] = {
    // FIXME check that ramps doen't exist

    val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))
    lazy val ramps: Vector[Ramp] = rampParser.loadRamps

    // FIXME Save ramps
    ramps
  }

}
