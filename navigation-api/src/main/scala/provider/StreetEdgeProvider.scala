package provider

import mapdomain.street.StreetEdge

import scala.collection.concurrent.TrieMap

trait StreetEdgeSupport {
  val streetEdgeProvider = StreetEdgeProvider
}

object StreetEdgeProvider extends GraphSupport {

  private val cache = new TrieMap[Long, StreetEdge]

  def findById(id: Long): StreetEdge = {
    val edge = graphs.street.vertices.find(v ⇒ v.edges.exists(e ⇒ e.id.contains(id))).get.edges.find(e ⇒ e.id.contains(id)).get
    cache.getOrElseUpdate(id, edge)
  }
}
