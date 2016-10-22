package provider

import mapdomain.repository.street.StreetRepositorySupport
import mapdomain.street.StreetInfo

import scala.collection.concurrent.TrieMap

trait StreetInfoSupport {

  val streetInfoProvider = StreetInfoProvider
}

object StreetInfoProvider extends StreetEdgeSupport with GraphSupport with StreetRepositorySupport {

  private val cache = new TrieMap[Long, StreetInfo]

  def findById(id: Long): StreetInfo = cache.getOrElseUpdate(id, streetInfoRepository.find(id))

  def findByStreetEdgeId(streetEdgeId: Long): StreetInfo = {
    val id = streetEdgeProvider.findById(streetEdgeId).streetInfoId
    findById(id)
  }
}
