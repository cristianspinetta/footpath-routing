package mapdomain.repository

import mapdomain.graph.Coordinate
import mapdomain.repository.publictransport.{ PathRepository, PublicTransportCombinationRepository, StopRepository, TravelInfoRepository }
import mapdomain.repository.sidewalk.{ RampRepository, SidewalkEdgeRepository, SidewalkVertexRepository, StreetCrossingEdgeRepository }
import mapdomain.repository.street.{ StreetEdgeRepository, StreetInfoRepository, StreetVertexRepository }
import org.scalatest.Matchers

trait BaseRepositoryDBSpec { self: Matchers ⇒

  def precision: Double

  def coordinateAssertion(coordResult: Coordinate, coordExpect: Coordinate): Unit = {
    coordResult.latitude should equal(coordExpect.latitude +- precision)
    coordResult.longitude should equal(coordExpect.longitude +- precision)
  }

  def deleteAll(): Unit = {
    StopRepository.deleteAll
    StreetInfoRepository.deleteAll
    StreetEdgeRepository.deleteAll
    StreetVertexRepository.deleteAll
    RampRepository.deleteAll
    PathRepository.deleteAll
    TravelInfoRepository.deleteAll
    SidewalkEdgeRepository.deleteAll
    StreetCrossingEdgeRepository.deleteAll
    SidewalkVertexRepository.deleteAll
    PublicTransportCombinationRepository.deleteAll
  }

}
