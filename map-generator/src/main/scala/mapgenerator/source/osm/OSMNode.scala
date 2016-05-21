package mapgenerator.source.osm

import org.joda.time.DateTime

case class OSMNode(lon: Double, lat: Double, uid: Long, user: String, timestamp: DateTime, changeset: Long,
    version: Int, visible: Boolean, id: Long, tags: Map[String, String]) {

  /**
   * Is this a public transport stop that can be linked to a transit stop vertex on the graph later on.
   * @return
   */
  def isStop: Boolean = {
    tags.get("highway").contains("bus_stop") ||
      tags.get("railway").contains("tram_stop") ||
      tags.get("railway").contains("station") ||
      tags.get("railway").contains("halt") ||
      tags.get("amenity").contains("bus_station")
  }

  def isBollard: Boolean = tags.get("barrier").contains("bollard")

  def isMultiLevel: Boolean = tags.get("highway").contains("elevator")
}

case class Way(id: Long, visible: Boolean, version: Int, changeset: Long, timestamp: DateTime, user: String,
    uid: Long, nodeIds: List[Long], tags: Map[String, String]) {
  def isSteps: Boolean = tags.get("highway").contains("steps")
}

case class Relation(id: Long, visible: Boolean, version: Int, changeset: Long, timestamp: DateTime, user: String,
  uid: Long, members: List[Member], tags: Map[String, String])

case class Member(`type`: String, ref: Long, role: String) // FIXME use Role and Type as Enum or object

case class Bounds(minlat: Double, minlon: Double, maxlat: Double, maxlon: Double)
