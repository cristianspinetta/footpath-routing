package module

import base.CaseObjectSerializationSupport
import mapdomain.sidewalk.Ramp
import model.{ RouteFormatter, _ }
import spray.json._

final case class RoutingRequest(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double, routingType: TypeRouting = SidewalkRouting)
object RoutingRequest {
  def applyWithDefault(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double): RoutingRequest = new RoutingRequest(fromLng, fromLat, toLng, toLat)
}
final case class EdgeRequest(edgeType: EdgeType, radius: Double, lat: Double, lng: Double)
final case class RampRequest(lat: Double, lng: Double, radius: Double)
final case class PublicTransportPathsRequest(lat: Double, lng: Double, radius: Double)
final case class StreetResponse(streets: Iterable[Street])
final case class SidewalkResponse(sidewalks: Iterable[Sidewalk])
final case class EdgeResponse(edges: Iterable[Edge], vertices: List[Vertex])
final case class RampResponse(ramps: Vector[Ramp])
final case class PublicTransportPathsResponse(PublicTransportPaths: List[PublicTransportPath])

trait Protocol extends DefaultJsonProtocol with CaseObjectSerializationSupport with ModelFormatter with RouteFormatter {

  implicit val EdgeRequestFormat = jsonFormat4(EdgeRequest.apply)
  implicit val RoutingRequestFormat = jsonFormat5(RoutingRequest.apply)
  implicit val RampRequestFormat = jsonFormat3(RampRequest.apply)
  implicit val PublicTransportPathsRequestFormat = jsonFormat3(PublicTransportPathsRequest.apply)
  implicit val PublicTransportPathsResponseFormat = jsonFormat1(PublicTransportPathsResponse.apply)
  implicit val StreetResponseFormat = jsonFormat1(StreetResponse.apply)
  implicit val RampResponseFormat = jsonFormat1(RampResponse.apply)
  implicit val SidewalkResponseFormat = jsonFormat1(SidewalkResponse.apply)
  implicit val EdgeResponseFormat = jsonFormat2(EdgeResponse.apply)

}

object Protocol extends Protocol
