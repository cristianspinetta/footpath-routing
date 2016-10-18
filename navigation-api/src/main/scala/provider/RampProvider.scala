package provider

import base.conf.ApiEnvConfig
import mapdomain.graph.Coordinate
import mapdomain.repository.sidewalk.{RampRepository, SidewalkVertexRepository, StreetCrossingEdgeRepository}
import mapdomain.sidewalk.{Ramp, StreetCrossingEdge}
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
    var crossingEdge: StreetCrossingEdge = null
    try {
      crossingEdge =StreetCrossingEdgeRepository.find(rampAssociation.streetCrossingEdgeId)
    } catch {
      case e: Exception => throw new RuntimeException(s"Unable to find edge ${rampAssociation.streetCrossingEdgeId}")
    }

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

}
