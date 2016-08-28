package service

import mapdomain.graph.Coordinate
import mapdomain.sidewalk.Ramp
import model.{ Edge, Sidewalk, Street }
import spray.json._

object Protocol extends DefaultJsonProtocol {

  implicit object CoordinateFormat extends RootJsonFormat[Coordinate] {
    def write(c: Coordinate) = JsObject(
      "lat" -> JsNumber(c.latitude),
      "lng" -> JsNumber(c.longitude))

    def read(value: JsValue) = value.asJsObject.getFields("lat", "lng") match {
      case Seq(JsNumber(latitude), JsNumber(longitude)) ⇒
        Coordinate(latitude.toDouble, longitude.toDouble)
      case _ ⇒ throw DeserializationException("Coordinate expected")
    }
  }

  implicit val RampFormat = jsonFormat5(Ramp.apply)
  implicit val EdgeFormat = jsonFormat2(Edge.apply)
  implicit val StreetFormat = jsonFormat3(Street.apply)
  implicit val SidewalkFormat = jsonFormat2(Sidewalk.apply)
  implicit val RoutingRequestFormat = jsonFormat4(RoutingRequest.apply)
  implicit val EdgeRequestFormat = jsonFormat1(EdgeRequest.apply)
  implicit val RoutingResponseFormat = jsonFormat1(RoutingResponse.apply)
  implicit val StreetResponseFormat = jsonFormat1(StreetResponse.apply)
  implicit val RampResponseFormat = jsonFormat1(RampResponse.apply)
  implicit val SidewalkResponseFormat = jsonFormat1(SidewalkResponse.apply)
  implicit val EdgeResponseFormat = jsonFormat1(EdgeResponse.apply)
}
