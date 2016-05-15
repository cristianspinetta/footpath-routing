package mapgenerator.source.osm

import org.joda.time.DateTime

case class Node(lon: Double, lat: Double, uid: Long, user: String, timestamp: DateTime, changeset: Long,
  version: Int, visible: Boolean, id: Long, tags: Map[String, String])

case class Way(id: Long, visible: Boolean, version: Int, changeset: Long, timestamp: DateTime, user: String,
  uid: Long, nodeIds: List[Long], tags: Map[String, String])

case class Relation(id: Long, visible: Boolean, version: Int, changeset: Long, timestamp: DateTime, user: String,
  uid: Long, members: List[Member], tags: Map[String, String])

case class Member(`type`: String, ref: Long, role: String) // FIXME use Role and Type as Enum or object

case class Bounds(minlat: Double, minlon: Double, maxlat: Double, maxlon: Double)
