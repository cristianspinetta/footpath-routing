package provider

import base.conf.ApiEnvConfig
import mapdomain.graph.Coordinate
import mapdomain.math.{GVector, Point, VectorUtils}
import mapdomain.repository.sidewalk.{RampRepository, SidewalkVertexRepository, StreetCrossingEdgeRepository}
import mapdomain.sidewalk.{Ramp, SidewalkVertex, StreetCrossingEdge}
import mapdomain.utils.EdgeUtils
import mapgenerator.source.features.{RampLoader, RampLoader2011, RampLoader2014, RampLoaderByCSV}

import scala.io.Source
import scala.sys.process.ProcessBuilder.Source

object RampProvider extends ApiEnvConfig {

  private lazy val rampPath2014: String = configuration.Ramp.sourceFile2014Path
  private lazy val rampPath2011: String = configuration.Ramp.sourceFile2011Path
  private lazy val rampAssociation: String = configuration.Ramp.sourceFileRampAssociationPath
  private lazy val rampParser: RampLoader = RampLoaderByCSV(Seq((rampPath2014, RampLoader2014), (rampPath2011, RampLoader2011)))

  lazy val ramps: Vector[Ramp] = rampParser.loadRamps

  case class RampAssociation(rampId: Long, vertexId: Long, streetCrossingEdgeId: Long)

  private def loadRampAssociations(): List[RampAssociation] = {
    Source.fromFile(rampAssociation).getLines().map(line => {
      val chucks = line.split(",")
      RampAssociation(chucks(0).toLong, chucks(1).toLong, chucks(2).toLong)
    }).toList
  }

  private def associate(rampAssociation: RampAssociation, edges: List[StreetCrossingEdge]): StreetCrossingEdge = {
    val crossingEdge: StreetCrossingEdge = edges.find(e => e.id.get == rampAssociation.streetCrossingEdgeId).getOrElse(
      try {
        StreetCrossingEdgeRepository.find(rampAssociation.streetCrossingEdgeId)
      } catch {
        case e: Exception => throw new RuntimeException(s"Unable to find edge ${rampAssociation.streetCrossingEdgeId}")
      }
    )

    var ramp: Ramp= null
    try {
      ramp = RampRepository.find(rampAssociation.rampId).get
    } catch {
      case e: Exception => throw new RuntimeException(s"Unable to find ramp ${rampAssociation.rampId}")
    }

    if (crossingEdge.vertexStartId == rampAssociation.vertexId)
      if (crossingEdge.rampStartId.isDefined || edges.exists(e => e.id == crossingEdge.id && e.rampStartId.isDefined && e.rampStartId == rampAssociation.vertexId))
        throw new RuntimeException(s"Cannot associate ramp ${rampAssociation.rampId} because crossing edge ${rampAssociation.streetCrossingEdgeId} has another ramp start associated")
      else
        crossingEdge.rampStartId = ramp.id
    else if (crossingEdge.vertexEndId == rampAssociation.vertexId)
      if (crossingEdge.rampEndId.isDefined || edges.exists(e => e.id == crossingEdge.id && e.rampEndId.isDefined && e.rampEndId == rampAssociation.vertexId))
        throw new RuntimeException(s"Cannot associate ramp ${rampAssociation.rampId} because crossing edge ${rampAssociation.streetCrossingEdgeId} has another ramp end associated")
      else
        crossingEdge.rampEndId = ramp.id
    else
      throw new RuntimeException(s"Invalid vertex id ${rampAssociation.vertexId}")

    crossingEdge
  }

  def associateRampsToSidewalks(): List[StreetCrossingEdge] = {
    val rampAssociations = loadRampAssociations
    val duplicatedVertexAndEdge = rampAssociations.groupBy(ra => (ra.vertexId, ra.streetCrossingEdgeId)).collect {
      case (x,ys) if ys.lengthCompare(1) > 0 => (x, ys.length)
    }

    if(duplicatedVertexAndEdge.size > 0)
      throw new RuntimeException(s"Vertex and edge with more than 1 ramp association:\n ${ duplicatedVertexAndEdge mkString "\n"}")

    val duplicatedRampAndVertex = rampAssociations.map(ra => (ra.rampId, ra.vertexId)).distinct.groupBy(rv => rv._1).collect {
      case (x,ys) if ys.lengthCompare(1) > 0 => (x, ys)
    }

    if(duplicatedRampAndVertex.size > 0)
      throw new RuntimeException(s"Ramps associated to more than one vertex:\n ${ duplicatedRampAndVertex mkString "\n"}")

    rampAssociations.foldLeft(List[StreetCrossingEdge]())((edges, rampAssociation) => associate(rampAssociation, edges) :: edges)
  }

  val rampDistanceToEdge: Double = {
    val c1 = Coordinate(-34.6124922, -58.4130873)
    val c2 = Coordinate(-34.6125422, -58.4130873)
    c1.distanceToInDegrees(c2)
  }

  private def processRamps(edges: List[StreetCrossingEdge], retrieveRampId: StreetCrossingEdge => Option[Long], retrieveVertexId: StreetCrossingEdge => Long): List[Ramp] = {
    edges.filter(e => retrieveRampId(e).isDefined).map(edge => {
      val edgesForRamp = StreetCrossingEdgeRepository.findCrossingEdgesByRamp(retrieveRampId(edge).get)
      edgesForRamp match {
        case h::Nil => moveRampCoordinate(retrieveRampId(edge).get, edge, retrieveVertexId(edge), rampDistanceToEdge, -rampDistanceToEdge)
        case h::t => moveRampCoordinate(retrieveRampId(edge).get, edge, retrieveVertexId(edge), -rampDistanceToEdge, -rampDistanceToEdge)
      }
    })
  }

  def updateRampCoordinates(edges: List[StreetCrossingEdge]): List[Ramp] = {
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
    val startRamps = processRamps(edges, e => e.rampStartId, e => e.vertexStartId)
    val endRamps = processRamps(edges, e => e.rampEndId, e => e.vertexEndId)

    startRamps ++ endRamps
  }

  private def moveRampCoordinate(rampId: Long, edge: StreetCrossingEdge, vertexId: Long, moveFromFirstEdge: Double, moveFromSecondEdge: Double): Ramp = {
    val ramp = RampRepository.find(rampId).get
    val vertexStart = SidewalkVertexRepository.find(vertexId).get
    val vertexEnd = findOppositeVertex(edge, vertexId)

    val otherEdgeAssociated = StreetCrossingEdgeRepository.findCrossingEdgesBySidewalkVertex(vertexId).find(e => e.id != edge.id).get
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
    if(vertexId == edge.vertexStartId)
      SidewalkVertexRepository.find(edge.vertexEndId).get
    else
      SidewalkVertexRepository.find(edge.vertexStartId).get
  }

}
