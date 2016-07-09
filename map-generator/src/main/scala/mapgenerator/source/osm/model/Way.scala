package mapgenerator.source.osm.model

import org.joda.time.DateTime

case class Way(id: Long, visible: Boolean, version: Int, changeset: Long, timestamp: DateTime, user: String,
    uid: Long, nodeIds: List[Long], tags: Map[String, String]) extends OSMElement {

  lazy val isSteps: Boolean = tags.get("highway").contains("steps")

  lazy val isAreaWay: Boolean = {
    (tags.get("area").contains("yes") ||
      tags.get("amenity").contains("parking") ||
      tags.get("amenity").contains("bicycle_parking")) &&
      nodeIds.size > 2
  }
}
