package mapdomain.repository.street

trait StreetRepositorySupport {
  val streetVertexRepository = StreetVertexRepository
  val streetEdgeRepository = StreetEdgeRepository
  val streetInfoRepository = StreetInfoRepository
}
