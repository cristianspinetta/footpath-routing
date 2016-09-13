package module

import java.net.URL

import base.{ LazyLoggerSupport, MeterSupport }
import conf.ApiEnvConfig
import mapdomain.sidewalk.{ EagerSidewalkGraphContainer, SidewalkRepositorySupport }
import mapdomain.street.{ EagerStreetGraphContainer, StreetRepositorySupport }
import mapgenerator.sidewalk.SidewalkModule
import mapgenerator.source.osm.{ OSMModule, OSMReaderByXml }
import mapgenerator.street.StreetGraphModule
import scalikejdbc.DB

import scala.util.Try

object MapGeneratorModule extends LazyLoggerSupport with MeterSupport with ApiEnvConfig with StreetRepositorySupport with SidewalkRepositorySupport {

  def createStreets(): Try[_] = Try {
    // FIXME Check that streets doen't exist.
    logger.info(s"Starting to create the streets")
    withTimeLogging({
      val osmURL: URL = getClass.getResource(configuration.OSM.sourceFilePath)
      val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)
      val osmModule: OSMModule = OSMModule(xmlParser)
      val streetGraphModule: StreetGraphModule = StreetGraphModule(osmModule)
      val streetGraph = streetGraphModule.createGraph.purgeStreets
      saveStreets(streetGraph)
    }, (time: Long) ⇒ logger.info(s"Created and saved the Street Graph in $time ms."))
  } flatten

  private def saveStreets(streetGraph: EagerStreetGraphContainer): Try[_] = Try {
    logger.info(s"Persist the street graph on the DB")
    withTimeLogging(
      DB localTx { implicit session ⇒
        val savedVertices = streetVertexRepository createInBulk streetGraph.vertices
        for {
          vertex ← savedVertices
          edge ← vertex.edges
        } {
          streetEdgeRepository.create(edge)
        }
      }, (time: Long) ⇒ logger.info(s"${streetGraph.vertices.size} vertices and ${streetGraph.vertices.map(_.edges.size).sum} edges for Street Graph saved in $time ms."))
  }

  def createSidewalks(failureTolerance: Boolean = true) = Try {
    logger.info(s"Starting to create the sidewalks")
    withTimeLogging({
      val sidewalkGraph = SidewalkModule()(EagerStreetGraphContainer.createFromDB)
        .createSideWalks(failureTolerance = failureTolerance).purgeSidewalks
      saveSidewalks(sidewalkGraph)
    }, (time: Long) ⇒ logger.info(s"Created and saved the Sidewalks Graph in $time ms."))
  } flatten

  private def saveSidewalks(sidewalkGraph: EagerSidewalkGraphContainer): Try[_] = Try {
    logger.info(s"Persist the sidewalk graph on the DB")
    withTimeLogging(
      DB localTx { implicit session ⇒
        sidewalkGraph.vertices foreach sidewalkVertexRepository.create
        sidewalkGraph.sidewalkEdges foreach sidewalkEdgeRepository.create
        sidewalkGraph.streetCrossingEdges foreach streetCrossingEdgeRepository.create
      }, (time: Long) ⇒
        logger.info(s"${sidewalkGraph.vertices.size} vertices, ${sidewalkGraph.sidewalkEdges.size} sidewalk edges and " +
          s"${sidewalkGraph.streetCrossingEdges.size} street crossing edges for Sidewalk Graph saved in $time ms."))
  }

}
