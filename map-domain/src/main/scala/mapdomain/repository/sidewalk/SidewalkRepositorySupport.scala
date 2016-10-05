package mapdomain.repository.sidewalk

trait SidewalkRepositorySupport {
  protected val sidewalkVertexRepository = SidewalkVertexRepository
  protected val sidewalkEdgeRepository = SidewalkEdgeRepository
  protected val streetCrossingEdgeRepository = StreetCrossingEdgeRepository
}

