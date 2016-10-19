package service

import base.{LazyLoggerSupport, MeterSupport}
import base.conf.ApiEnvConfig
import mapdomain.repository.sidewalk.{RampRepository, SidewalkRepositorySupport, StreetCrossingEdgeRepository}
import mapdomain.repository.street.{StreetInfoRepository, StreetRepositorySupport}
import mapdomain.sidewalk.{InMemorySidewalkGraphContainer, Ramp, StreetCrossingEdge}
import mapdomain.street._
import mapdomain.utils.EdgeUtils
import mapgenerator.sidewalk.SidewalkModule
import mapgenerator.source.osm.{OSMModule, OSMReaderByXml}
import mapgenerator.street.StreetGraphModule
import provider.RampProvider
import scalikejdbc.DB

import scala.collection.concurrent.TrieMap
import scala.util.Try

trait MapGeneratorService extends LazyLoggerSupport with MeterSupport with ApiEnvConfig with StreetRepositorySupport with SidewalkRepositorySupport {

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

  def createRamps() = Try {
    logger.info(s"Starting to create ramps")
    withTimeLogging({
      saveRamps(RampProvider.ramps)
    }, (time: Long) ⇒ logger.info(s"Created and saved ramps in $time ms."))
  }

  private def saveRamps(ramps: Vector[Ramp]) = Try {
    DB localTx { implicit session ⇒
      ramps foreach RampRepository.createRamp
    }
  }

  def associateRampsToSidewalks() = Try {
    logger.info(s"Starting to associate ramps to sidewalks")
    withTimeLogging({
      val edges = RampProvider.associateRampsToSidewalks
      updateStreetCrossingEdges(edges)
      val ramps = RampProvider.updateRampCoordinates(edges)
      updateRamps(ramps)
    }, (time: Long) ⇒ logger.info(s"Associated ramps to sidewalks in $time ms."))
  }

  private def updateStreetCrossingEdges(edges: List[StreetCrossingEdge]) = Try {
    DB localTx { implicit session ⇒
      edges foreach StreetCrossingEdgeRepository.save
    }
  }

  private def updateRamps(ramps: List[Ramp]) = Try {
    DB localTx { implicit session ⇒
      ramps foreach RampRepository.save
    }
  }

}

object MapGeneratorService extends MapGeneratorService
