package mapgenerator.source.osm.model

import mapdomain.graph.{Coordinate, GeoVertex, GraphContainer}
import mapdomain.street.OsmVertex
import org.joda.time.DateTime

case class Way(id: Long, visible: Boolean, version: Int, changeset: Long, timestamp: DateTime, user: String,
    uid: Long, nodeIds: List[Long], tags: Map[String, String]) extends OSMElement {

  import OSMElement._

  lazy val isSteps: Boolean = tags.get("highway").contains("steps")

  lazy val isRoundabout: Boolean = tags.get("junction").contains("roundabout")

  lazy val isOneWayForwardDriving: Boolean = isTagTrue(tags, "oneway")
  lazy val isOneWayReverseDriving: Boolean = tags.get("oneway").contains("-1")

  lazy val isOneWayForwardBicycle: Boolean = isTagTrue(tags, "oneway:bicycle") || isTagFalse(tags, "bicycle:backwards")
  lazy val isOneWayReverseBicycle: Boolean = tags.get("oneway:bicycle").contains("-1")

  lazy val isForwardDirectionSidepath: Boolean = tags.get("bicycle:forward").contains("use_sidepath")
  lazy val isReverseDirectionSidepath: Boolean = tags.get("bicycle:backward").contains("use_sidepath")

  lazy val isOpposableCycleway: Boolean = {
    tags.get("cycleway").exists(_.startsWith("opposite")) ||
      tags.get("cycleway:left").exists(_.startsWith("opposite")) ||
      tags.get("cycleway:right").exists(_.startsWith("opposite"))
  }

  lazy val isAreaWay: Boolean = {
    (tags.get("area").contains("yes") ||
      tags.get("amenity").contains("parking") ||
      tags.get("amenity").contains("bicycle_parking")) &&
      nodeIds.size > 2
  }
}

object Way {

  def createOSMVertex(way: Way, node: OSMNode): OsmVertex = new OsmVertex(node.id, Nil, Coordinate(node.lat, node.lon))

  def getPath[V <: GeoVertex](way: Way)(implicit graph: GraphContainer[V]): List[Coordinate] = {
    for {
      nodeId <- way.nodeIds
      vertex <- graph.findVertex(nodeId).toSeq
    } yield vertex.coordinate
  }
}
