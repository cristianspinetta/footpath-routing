package module

import base.CaseObjectSerializationSupport
import mapdomain.sidewalk.Ramp
import model.{ ReportableElement, RouteFormatter, _ }
import spray.json._

final case class RoutingRequest(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double, heuristicType: HeuristicType, routingType: TypeRouting = SidewalkRouting)
object RoutingRequest {
  def applyWithDefault(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double, heuristicType: HeuristicType): RoutingRequest = new RoutingRequest(fromLng, fromLat, toLng, toLat, heuristicType)
}
final case class EdgeRequest(edgeType: EdgeType, radius: Double, lat: Double, lng: Double)
final case class RampRequest(lat: Double, lng: Double, radius: Double, associated: Boolean)
final case class ReportableElementsRequest(northeast: String, southwest: String)
final case class PublicTransportPathsRequest(lat: Double, lng: Double, radius: Option[Double], line: Option[String]) // extends WithValidatedRadius {
//  protected val _radius: Double = radius.getOrElse(0)
//  protected val _maxRadiusAllow: Double = 1
//}
final case class PublicTransportCombinationsRequest(lat: Double, lng: Double, radius: Double)
final case class StreetResponse(streets: Iterable[Street])
final case class SidewalkResponse(sidewalks: Iterable[Sidewalk])
final case class EdgeResponse(edges: Iterable[Edge], vertices: List[Vertex])
final case class RampResponse(ramps: List[Ramp])
final case class ReportableElementsResponse(elements: Vector[ReportableElement])
final case class UpdateStopRequest(id: Long, enabled: Boolean)

final case class PublicTransportCreationRequest(limit: String, offset: String)

trait WithValidatedRadius {
  protected def _maxRadiusAllow: Double
  protected def _radius: Double
  assert(_radius <= _maxRadiusAllow, s"the supplied radius is greater than the radius allowed. [radius: ${_radius}, maxRadius: ${_maxRadiusAllow}]")
}

trait Protocol extends DefaultJsonProtocol with CaseObjectSerializationSupport with ModelFormatter with RouteFormatter {

  implicit val EdgeRequestFormat = jsonFormat4(EdgeRequest.apply)
  implicit val RoutingRequestFormat = jsonFormat6(RoutingRequest.apply)
  implicit val RampRequestFormat = jsonFormat4(RampRequest.apply)
  implicit val ReportableElementsRequestFormat = jsonFormat2(ReportableElementsRequest.apply)
  implicit val PublicTransportPathsRequestFormat = jsonFormat4(PublicTransportPathsRequest.apply)
  implicit val PublicTransportCombinationsRequestFormat = jsonFormat3(PublicTransportCombinationsRequest.apply)
  implicit val StreetResponseFormat = jsonFormat1(StreetResponse.apply)
  implicit val RampResponseFormat = jsonFormat1(RampResponse.apply)
  implicit val SidewalkResponseFormat = jsonFormat1(SidewalkResponse.apply)
  implicit val EdgeResponseFormat = jsonFormat2(EdgeResponse.apply)
  implicit val ReportableElementsResponseFormat = jsonFormat1(ReportableElementsResponse.apply)
  implicit val PublicTransportCreationRequestFormat = jsonFormat2(PublicTransportCreationRequest.apply)
  implicit val UpdateStopRequestFormat = jsonFormat2(UpdateStopRequest.apply)

}

object Protocol extends Protocol
