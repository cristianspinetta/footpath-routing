package service

import base.{ LazyLoggerSupport, MeterSupport }
import base.conf.ApiEnvConfig
import mapdomain.sidewalk.{ InMemorySidewalkGraphContainer, SidewalkRepositorySupport }
import mapdomain.street._
import mapgenerator.sidewalk.SidewalkModule
import mapgenerator.source.osm.{ OSMModule, OSMReaderByXml }
import mapgenerator.street.StreetGraphModule
import scalikejdbc.DB

import scala.collection.concurrent.TrieMap
import scala.util.Try

object MapGeneratorService extends LazyLoggerSupport with MeterSupport with ApiEnvConfig with StreetRepositorySupport with SidewalkRepositorySupport {

  def createStreets(): Try[_] = Try {
    // FIXME Check that streets doen't exist.
    logger.info(s"Starting to create the streets")
    withTimeLogging({
      val osmURL: String = configuration.OSM.sourceFilePath
      val xmlParser: OSMReaderByXml = OSMReaderByXml(osmURL)
      val osmModule: OSMModule = OSMModule(xmlParser)
      val streetGraphModule: StreetGraphModule = StreetGraphModule(osmModule)
      val streetGraph = streetGraphModule.createGraph.purgeStreets
      logger.debug(s"StreetGraph created. Vertices: ${streetGraph.vertices.size}. Street Edges: ${streetGraph.vertices.map(_.edges.size).sum}.")
      saveStreets(streetGraph)
    }, (time: Long) ⇒ logger.info(s"Created and saved the Street Graph in $time ms."))
  } flatten

  private def saveStreets(streetGraph: UnsavedStreetGraphContainer): Try[_] = Try {
    logger.info(s"Persist the street graph on the DB")
    withTimeLogging(
      DB localTx { implicit session ⇒
        logger.info(s"Inserting street vertices...")
        streetVertexRepository createInBulk streetGraph.vertices
        logger.info(s"Inserting street edges...")
        val streetInfoByWayId = new TrieMap[Long, Long]()
        for {
          vertex ← streetGraph.vertices
          edge ← vertex.edges
        } {
          val savedStreetInfoId: Long = streetInfoByWayId.getOrElse(edge.wayId, {
            val id = StreetInfoRepository.create(edge.streetInfo)
            streetInfoByWayId += (edge.wayId -> id)
            id
          })
          streetEdgeRepository.create(edge.build(savedStreetInfoId))
        }
      }, (time: Long) ⇒ logger.info(s"${streetGraph.vertices.size} vertices and ${streetGraph.vertices.map(_.edges.size).sum} edges for Street Graph saved in $time ms."))
  }

  def createSidewalks(failureTolerance: Boolean = true) = Try {
    logger.info(s"Starting to create the sidewalks")
    withTimeLogging({
      val sidewalkGraph = SidewalkModule()(InMemoryStreetGraphContainer.createFromDB)
        .createSideWalks(failureTolerance = failureTolerance).purgeSidewalks
      logger.debug(s"sidewalkGraph created. Vertices: ${sidewalkGraph.vertices.size}. Sidewalk Edges: ${sidewalkGraph.sidewalkEdges.size}. Street Crossing Edges: ${sidewalkGraph.streetCrossingEdges.size}")
      saveSidewalks(sidewalkGraph)
    }, (time: Long) ⇒ logger.info(s"Created and saved the Sidewalks Graph in $time ms."))
  } flatten

  private def saveSidewalks(sidewalkGraph: InMemorySidewalkGraphContainer): Try[_] = Try {
    logger.info(s"Persist the sidewalk graph on the DB")
    withTimeLogging(
      DB localTx { implicit session ⇒
        logger.info(s"Inserting sidewalk vertices...")
        sidewalkGraph.vertices foreach sidewalkVertexRepository.create
        logger.info(s"Inserting sidewalk edges...")
        sidewalkGraph.sidewalkEdges foreach sidewalkEdgeRepository.create
        logger.info(s"Inserting street crossing edges...")
        sidewalkGraph.streetCrossingEdges foreach streetCrossingEdgeRepository.create
      }, (time: Long) ⇒
        logger.info(s"${sidewalkGraph.vertices.size} vertices, ${sidewalkGraph.sidewalkEdges.size} sidewalk edges and " +
          s"${sidewalkGraph.streetCrossingEdges.size} street crossing edges for Sidewalk Graph saved in $time ms."))
  }

}
