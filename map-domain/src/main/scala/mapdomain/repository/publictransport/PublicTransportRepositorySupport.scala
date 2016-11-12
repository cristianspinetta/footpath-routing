package mapdomain.repository.publictransport

trait PublicTransportRepositorySupport {

  protected val stopRepository: StopRepository = StopRepository
  protected val travelInfoRepository: TravelInfoRepository = TravelInfoRepository
  protected val pathRepository: PathRepository = PathRepository
  protected val publicTransportCombinationRepository: PublicTransportCombinationRepository = PublicTransportCombinationRepository

}
