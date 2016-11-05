package service

import base.{ LazyLoggerSupport, MeterSupport }
import base.conf.ApiEnvConfig
import cats.data.Xor
import mapdomain.graph.Coordinate
import mapdomain.math.{ GVector, Point, VectorUtils }
import mapdomain.publictransport.PublicTransportCombination
import mapdomain.repository.publictransport.{ PublicTransportCombinationRepository, StopRepository }
import mapdomain.repository.sidewalk.{ RampRepository, SidewalkRepositorySupport, SidewalkVertexRepository, StreetCrossingEdgeRepository }
import mapdomain.repository.street.{ StreetInfoRepository, StreetRepositorySupport }
import mapdomain.sidewalk.{ InMemorySidewalkGraphContainer, Ramp, SidewalkVertex, StreetCrossingEdge }
import mapdomain.street._
import mapgenerator.sidewalk.SidewalkModule
import mapgenerator.source.features.{ RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV }
import mapgenerator.source.osm.{ OSMModule, OSMReaderByXml }
import mapgenerator.street.StreetGraphModule
import model.{ AccessibilityHeuristicType, Path }
import provider.GraphSupport
import scalikejdbc.DB
import searching.WalkRouteSearcher
import snapshot.ObjectSerializer

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Try

trait MapGeneratorServiceSupport {
  val mapGeneratorService: MapGeneratorService = MapGeneratorService
}

trait MapGeneratorService extends LazyLoggerSupport with MeterSupport with ApiEnvConfig with GraphSupport with StreetRepositorySupport with SidewalkRepositorySupport {

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
      val sidewalkGraph = SidewalkModule()(graphs.streetDB)
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
      saveRamps(getRampsFromFile)
    }, (time: Long) ⇒ logger.info(s"Created and saved ramps in $time ms."))
  }

  def processCombinationsWalkPaths(limit: Integer, offset: Integer)(implicit ec: ExecutionContext) = Try {
    logger.info(s"Starting to process combinations")
    withTimeLogging({
      saveCombinations(updateCombinationsPath(limit, offset))
    }, (time: Long) ⇒ logger.info(s"Created and saved ramps in $time ms."))
  }

  private def updateCombinationsPath(limit: Integer, offset: Integer)(implicit ec: ExecutionContext): List[PublicTransportCombination] = {
    val combinations = PublicTransportCombinationRepository.findLimitted(limit, offset)
    combinations.map(c ⇒ {
      val from = StopRepository.find(c.fromStopId).get
      val to = StopRepository.find(c.toStopId).get
      val searchPath = WalkRouteSearcher.synchronicSearch(from.coordinate, to.coordinate, AccessibilityHeuristicType)
      searchPath match {
        case Some(path) ⇒ c.copy(walkPath = Some(ObjectSerializer.serialize(path)))
        case _          ⇒ c
      }
    })
  }

  private def saveCombinations(ptcs: List[PublicTransportCombination]) = Try {
    DB localTx { implicit session ⇒
      ptcs foreach PublicTransportCombinationRepository.save
    }
  }

  private def getRampsFromFile: Vector[Ramp] = {

    lazy val rampPath2014: String = configuration.Ramp.sourceFile2014Path
    lazy val rampPath2011: String = configuration.Ramp.sourceFile2011Path
    lazy val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

    rampParser.loadRamps
  }

  private def saveRamps(ramps: Vector[Ramp]) = Try {
    DB localTx { implicit session ⇒
      ramps foreach RampRepository.createRamp
    }
  }

  case class RampAssociation(rampId: Long, vertexId: Long, streetCrossingEdgeId: Long)

  private def loadRampAssociations(): List[RampAssociation] = {
    lazy val rampAssociation: String = configuration.Ramp.sourceFileRampAssociationPath
    Source.fromFile(rampAssociation).getLines().map(line ⇒ {
      val chucks = line.split(",")
      RampAssociation(chucks(0).toLong, chucks(1).toLong, chucks(2).toLong)
    }).toList
  }

  def associateRamps() = Try {
    logger.info(s"Starting to associate ramps to sidewalks")
    withTimeLogging({
      val edges = associateRampsToSidewalks
      updateStreetCrossingEdges(edges)
      val ramps = updateRampCoordinates(edges)
      updateRamps(ramps)
    }, (time: Long) ⇒ logger.info(s"Associated ramps to sidewalks in $time ms."))
  }

  private def associate(rampAssociation: RampAssociation, edges: List[StreetCrossingEdge]): StreetCrossingEdge = {
    val crossingEdge: StreetCrossingEdge = edges.find(e ⇒ e.id.get == rampAssociation.streetCrossingEdgeId).getOrElse(
      try {
        StreetCrossingEdgeRepository.find(rampAssociation.streetCrossingEdgeId)
      } catch {
        case e: Exception ⇒ throw new RuntimeException(s"Unable to find edge ${rampAssociation.streetCrossingEdgeId}")
      })

    var ramp: Ramp = null
    try {
      ramp = RampRepository.find(rampAssociation.rampId).get
    } catch {
      case e: Exception ⇒ throw new RuntimeException(s"Unable to find ramp ${rampAssociation.rampId}")
    }

    if (crossingEdge.vertexStartId == rampAssociation.vertexId)
      /*if (crossingEdge.rampStartId.isDefined || edges.exists(e => e.id == crossingEdge.id && e.rampStartId.isDefined && e.rampStartId == rampAssociation.vertexId))
          throw new RuntimeException(s"Cannot associate ramp ${rampAssociation.rampId} because crossing edge ${rampAssociation.streetCrossingEdgeId} has another ramp start associated")
        else*/
      crossingEdge.rampStartId = ramp.id
    else if (crossingEdge.vertexEndId == rampAssociation.vertexId)
      /*if (crossingEdge.rampEndId.isDefined || edges.exists(e => e.id == crossingEdge.id && e.rampEndId.isDefined && e.rampEndId == rampAssociation.vertexId))
        throw new RuntimeException(s"Cannot associate ramp ${rampAssociation.rampId} because crossing edge ${rampAssociation.streetCrossingEdgeId} has another ramp end associated")
      else*/
      crossingEdge.rampEndId = ramp.id
    else
      throw new RuntimeException(s"Invalid vertex id ${rampAssociation.vertexId}")

    crossingEdge
  }

  private def associateRampsToSidewalks(): List[StreetCrossingEdge] = {
    val rampAssociations = loadRampAssociations

    /*
    val duplicatedVertexAndEdge = rampAssociations.groupBy(ra => (ra.vertexId, ra.streetCrossingEdgeId)).collect {
      case (x,ys) if ys.lengthCompare(1) > 0 => (x, ys.length)
    }

    if(duplicatedVertexAndEdge.size > 0)
      throw new RuntimeException(s"Vertex and edge with more than 1 ramp association:\n ${ duplicatedVertexAndEdge mkString "\n"}")
    */

    val duplicatedRampAndVertex = rampAssociations.map(ra ⇒ (ra.rampId, ra.vertexId)).distinct.groupBy(rv ⇒ rv._1).collect {
      case (x, ys) if ys.lengthCompare(1) > 0 ⇒ (x, ys)
    }

    if (duplicatedRampAndVertex.size > 0)
      throw new RuntimeException(s"Ramps associated to more than one vertex:\n ${duplicatedRampAndVertex mkString "\n"}")

    rampAssociations.foldLeft(List[StreetCrossingEdge]())((edges, rampAssociation) ⇒ associate(rampAssociation, edges) :: edges)
  }

  val rampDistanceToEdge: Double = {
    val c1 = Coordinate(-34.6124922, -58.4130873)
    val c2 = Coordinate(-34.6125422, -58.4130873)
    c1.distanceToInDegrees(c2)
  }

  private def processRamps(edges: List[StreetCrossingEdge], retrieveRampId: StreetCrossingEdge ⇒ Option[Long], retrieveVertexId: StreetCrossingEdge ⇒ Long): List[Ramp] = {
    edges.filter(e ⇒ retrieveRampId(e).isDefined).map(edge ⇒ {
      val edgesForRamp = StreetCrossingEdgeRepository.findCrossingEdgesByRamp(retrieveRampId(edge).get)
      edgesForRamp match {
        case h :: Nil ⇒ moveRampCoordinate(retrieveRampId(edge).get, edge, retrieveVertexId(edge), rampDistanceToEdge, -rampDistanceToEdge)
        case h :: t   ⇒ moveRampCoordinate(retrieveRampId(edge).get, edge, retrieveVertexId(edge), -rampDistanceToEdge, -rampDistanceToEdge)
      }
    })
  }

  private def updateRampCoordinates(edges: List[StreetCrossingEdge]): List[Ramp] = {
    /*por cada edge y por cada rampa (start y end):
    * 1- si otro edge no tiene la misma rampa asociada:
    *     . buscar punto a una distancia d del edge al que pertenece la rampa
    *     . buscar punto a una distancia d del edge con el mismo vertex que el de la rampa a evaluar, pero en el otro sentido (d negativo)
    *     . de los anteriores, sacar la latitud y longitud de la rampa
    * 2- si otro edge tiene la misma rampa asociada:
    *     . buscar punto a una distancia d del edge al que pertenece la rampa, pero en el otro sentido (d negativo)
    *     . buscar punto a una distancia d del edge con el mismo vertex que el de la rampa a evaluar, pero en el otro sentido (d negativo)
    *     . de los anteriores, sacar la latitud y longitud de la rampa
    */
    val startRamps = processRamps(edges, e ⇒ e.rampStartId, e ⇒ e.vertexStartId)
    val endRamps = processRamps(edges, e ⇒ e.rampEndId, e ⇒ e.vertexEndId)

    startRamps ++ endRamps
  }

  private def moveRampCoordinate(rampId: Long, edge: StreetCrossingEdge, vertexId: Long, moveFromFirstEdge: Double, moveFromSecondEdge: Double): Ramp = {
    val ramp = RampRepository.find(rampId).get
    val vertexStart = SidewalkVertexRepository.find(vertexId).get
    val vertexEnd = findOppositeVertex(edge, vertexId)

    val otherEdgeAssociated = StreetCrossingEdgeRepository.findCrossingEdgesBySidewalkVertex(vertexId).find(e ⇒ e.id != edge.id).get
    val otherVertexEnd = findOppositeVertex(otherEdgeAssociated, vertexId)

    val pointStart = Point(vertexStart.coordinate.longitude, vertexStart.coordinate.latitude)
    var pointEnd = Point(vertexEnd.coordinate.longitude, vertexEnd.coordinate.latitude)
    val startVector: GVector = GVector(pointStart, pointEnd)

    pointEnd = Point(otherVertexEnd.coordinate.longitude, otherVertexEnd.coordinate.latitude)
    val endVector: GVector = GVector(pointStart, pointEnd)

    val longitude = VectorUtils.findPointAtDistance(moveFromFirstEdge, startVector).x
    val latitude = VectorUtils.findPointAtDistance(moveFromSecondEdge, endVector).y

    ramp.coordinate = Coordinate(latitude, longitude)
    ramp
  }

  private def findOppositeVertex(edge: StreetCrossingEdge, vertexId: Long): SidewalkVertex = {
    if (vertexId == edge.vertexStartId)
      SidewalkVertexRepository.find(edge.vertexEndId).get
    else
      SidewalkVertexRepository.find(edge.vertexStartId).get
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
