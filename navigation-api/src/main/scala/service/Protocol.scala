package service

import base.CaseObjectSerializationSupport
import model.{ RouteFormatter, _ }
import spray.json._

trait Protocol extends DefaultJsonProtocol with CaseObjectSerializationSupport with ModelFormatter with RouteFormatter {

  implicit val EdgeRequestFormat = jsonFormat4(EdgeRequest.apply)
  implicit val RoutingRequestFormat = jsonFormat5(RoutingRequest.apply)
  implicit val RampRequestFormat = jsonFormat3(RampRequest.apply)
  implicit val RoutingResponseFormat = jsonFormat1(RoutingResponse.apply)
  implicit val StreetResponseFormat = jsonFormat1(StreetResponse.apply)
  implicit val RampResponseFormat = jsonFormat1(RampResponse.apply)
  implicit val SidewalkResponseFormat = jsonFormat1(SidewalkResponse.apply)
  implicit val EdgeResponseFormat = jsonFormat1(EdgeResponse.apply)

}

object Protocol extends Protocol
