package mapdomain.repository.publictransport

trait PublicTransportRepositorySupport {

  protected def stopRepository = StopRepository
  protected def travelInfoRepository = TravelInfoRepository
  protected def pathRepository = PathRepository

}
