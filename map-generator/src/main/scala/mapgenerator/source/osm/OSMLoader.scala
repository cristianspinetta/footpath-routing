package mapgenerator.source.osm

import org.joda.time.DateTime

import java.net.URL
import scala.xml.{Elem, XML}

trait OSMLoader {
  def loadNodes: Seq[OSMNode]
  def loadWays: Seq[Way]
  def loadRelations: Seq[Relation]
}

class OSMLoaderByXml(osm: Elem) extends OSMLoader {

  lazy val loadNodes: Seq[OSMNode] = (osm \ "node").map {
    case n ⇒
      OSMNode(
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

  lazy val loadWays: Seq[Way] = (osm \ "way").map {
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

  lazy val loadRelations: Seq[Relation] = (osm \ "relation").map {
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

object OSMLoaderByXml {

  def apply(url: URL): OSMLoaderByXml = new OSMLoaderByXml(XML.load(url.getPath))

  def apply(systemFilePath: String): OSMLoaderByXml = new OSMLoaderByXml(XML.load(systemFilePath))
}
