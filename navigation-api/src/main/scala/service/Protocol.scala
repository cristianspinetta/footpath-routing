package service

import mapgenerator.source.osm.graph.Ramp
import pathgenerator.graph.Coordinate
import spray.json._

object Protocol extends DefaultJsonProtocol {

  implicit object CoordinateFormat extends RootJsonFormat[Coordinate] {
    def write(c: Coordinate) = JsObject(
      "lat" -> JsNumber(c.latitude),
      "lng" -> JsNumber(c.longitude))

    def read(value: JsValue) = value.asJsObject.getFields("lat", "lng") match {
      case Seq(JsNumber(latitude), JsNumber(longitude)) ⇒
        Coordinate(latitude.toDouble, longitude.toDouble)
      case _ ⇒ throw new DeserializationException("Coordinate expected")
    }
  }

  implicit val RampFormat = jsonFormat5(Ramp.apply)
  implicit val RoutingRequestFormat = jsonFormat4(RoutingRequest.apply)
  implicit val RoutingResponseFormat = jsonFormat1(RoutingResponse.apply)
  implicit val RampResponseFormat = jsonFormat1(RampResponse.apply)
}
