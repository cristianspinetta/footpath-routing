package mapdomain.repository.publictransport

trait PublicTransportRepositorySupport {

  protected def stopRepository: StopRepository = StopRepository
  protected def travelInfoRepository: TravelInfoRepository = TravelInfoRepository
  protected def pathRepository: PathRepository = PathRepository

}
