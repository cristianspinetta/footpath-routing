package mapgenerator.source.osm

import org.joda.time.DateTime

import scala.xml.{ Elem, XML }

trait OSMLoader {
  def loadNodes: Seq[Node]
  def loadWays: Seq[Way]
  def loadRelations: Seq[Relation]
}

case class OSMLoaderByXml(filePath: String) extends OSMLoader {

  val osm: Elem = XML.load("/home/cristian/Documents/Development/footpath-routing/map.osm")

  override def loadNodes: Seq[Node] = (osm \ "node").map {
    case n ⇒
      Node(
        lon = (n \ "@lon").text.toDouble,
        lat = (n \ "@lat").text.toDouble,
        uid = (n \ "@uid").text.toLong,
        user = (n \ "@user").text,
        timestamp = DateTime.parse((n \ "@timestamp").text),
        changeset = (n \ "@changeset").text.toLong,
        version = (n \ "@version").text.toInt,
        visible = (n \ "@visible").text.toBoolean,
        id = (n \ "@id").text.toLong,
        tags = (n \ "tag").map(tag ⇒
          (tag \ "@k").text -> (tag \ "@v").text).toMap)
  }

  override def loadWays: Seq[Way] = (osm \ "way").map {
    case w ⇒
      Way(
        id = (w \ "@id").text.toLong,
        visible = (w \ "@visible").text.toBoolean,
        version = (w \ "@version").text.toInt,
        changeset = (w \ "@changeset").text.toLong,
        timestamp = DateTime.parse((w \ "@timestamp").text),
        user = (w \ "@user").text,
        uid = (w \ "@uid").text.toLong,
        nodeIds = (w \ "nd").map(node ⇒
          (node \ "@ref").text.toLong).toList,
        tags = (w \ "tag").map(tag ⇒
          (tag \ "@k").text -> (tag \ "@v").text).toMap)
  }

  override def loadRelations: Seq[Relation] = (osm \ "relation").map {
    case r ⇒
      Relation(
        id = (r \ "@id").text.toLong,
        visible = (r \ "@visible").text.toBoolean,
        version = (r \ "@version").text.toInt,
        changeset = (r \ "@changeset").text.toLong,
        timestamp = DateTime.parse((r \ "@timestamp").text),
        user = (r \ "@user").text,
        uid = (r \ "@uid").text.toLong,
        members = (r \ "member").map(member ⇒
          Member(
            (member \ "@type").text,
            (member \ "@ref").text.toLong,
            (member \ "@role").text)).toList,
        tags = (r \ "tag").map(tag ⇒
          (tag \ "@k").text -> (tag \ "@v").text).toMap)
  }
}
