package mapgenerator.source.osm

import java.net.URL

import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

case class GraphJsonLoader(jValue: JValue) {

  implicit val formats = DefaultFormats

  lazy val vertices: List[OTPVertex] = jValue.extract[List[OTPVertex]]

}

object GraphJsonLoader {

  def apply(url: URL): GraphJsonLoader = {
    new GraphJsonLoader(parse(Source.fromFile(url.toURI).getLines().mkString))
  }
}

case class OTPVertex(index: Long, label: String, name: String, x: Double, y: Double, trafficLight: Boolean,
                     freeFlowing: Boolean, nodeId: Long, incomingStreetEdges: List[OTPEdge],
                     outgoingStreetEdges: List[OTPEdge])

case class OTPEdge(id: Long,
  bicycleSafetyFactor: Double,
  name: String,
  permission: String,
  wayId: Long,
  streetClass: Long,
  carSpeed: Double,
  inAngle: Double,
  outAngle: Double,
  roundabout: Boolean,
  distance: Double,
  elevationProfile: Option[Any],
  elevationFlattened: Boolean,
  maxSlope: Double,
  slopeSpeedEffectiveLength: Double,
  slopeWorkCostEffectiveLength: Double,
  wheelchairAccessible: Boolean,
  back: Boolean,
  noThruTraffic: Boolean,
  stairs: Boolean,
  slopeOverride: Boolean,
  startOsmNodeId: Long,
  endOsmNodeId: Long,
  direction: Option[Any],
  trip: Option[Any],
  partial: Boolean)
