package mapgenerator.source.osm.model

import org.joda.time.DateTime

case class Relation(id: Long, visible: Boolean, version: Int, changeset: Long, timestamp: DateTime, user: String,
  uid: Long, members: List[Member], tags: Map[String, String]) extends OSMElement
