package mapgenerator.source.osm

import java.net.URL

import base.LazyLoggerSupport
import mapgenerator.source.osm.model._
import org.joda.time.DateTime

import scala.xml.{ Elem, XML }

trait OSMReader {
  def loadNodes: Seq[OSMNode]
  def loadWays: Seq[Way]
  def loadRelations: Seq[Relation]
}

class OSMReaderByXml(osm: Elem) extends OSMReader {

  private def toSecureBoolean(value: String): Boolean = Option(value).contains("true")

  lazy val loadNodes: Seq[OSMNode] = (osm \ "node").map { n ⇒
    OSMNode(
      lon = (n \ "@lon").text.toDouble,
      lat = (n \ "@lat").text.toDouble,
      uid = (n \ "@uid").text.toLong,
      user = (n \ "@user").text,
      timestamp = DateTime.parse((n \ "@timestamp").text),
      changeset = (n \ "@changeset").text.toLong,
      version = (n \ "@version").text.toInt,
      visible = toSecureBoolean((n \ "@visible").text),
      id = (n \ "@id").text.toLong,
      tags = (n \ "tag").map(tag ⇒
        (tag \ "@k").text -> (tag \ "@v").text).toMap)
  }

  lazy val loadWays: Seq[Way] = (osm \ "way").map { w ⇒
    Way(
      id = (w \ "@id").text.toLong,
      visible = toSecureBoolean((w \ "@visible").text),
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

  lazy val loadRelations: Seq[Relation] = (osm \ "relation").map { r ⇒
    Relation(
      id = (r \ "@id").text.toLong,
      visible = toSecureBoolean((r \ "@visible").text),
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

object OSMReaderByXml extends LazyLoggerSupport {

  def apply(url: URL): OSMReaderByXml = {
    logLoading(url.getPath)
    new OSMReaderByXml(XML.load(url.getPath))
  }

  def apply(systemFilePath: String): OSMReaderByXml = {
    logLoading(systemFilePath)
    new OSMReaderByXml(XML.load(systemFilePath))
  }

  private def logLoading(path: String): Unit = {
    logger.info(s"Loading OSM file from $path")
  }
}
