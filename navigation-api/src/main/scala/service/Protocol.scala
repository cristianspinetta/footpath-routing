package service

import pathgenerator.graph.Coordinate
import spray.json._

object Protocol extends DefaultJsonProtocol {

  implicit object CoordinateFormat extends RootJsonFormat[Coordinate] {
    def write(c: Coordinate) = JsObject(
      "latitude" -> JsNumber(c.latitude),
      "longitude" -> JsNumber(c.longitude)
    )

    def read(value: JsValue) = value.asJsObject.getFields("latitude", "longitude") match {
      case Seq(JsNumber(latitude), JsNumber(longitude)) =>
        Coordinate(latitude.toDouble, longitude.toDouble)
      case _ => throw new DeserializationException("Color expected")
    }
  }

  implicit val RoutingRequestFormat = jsonFormat2(RoutingRequest.apply)
  implicit val RoutingResponseFormat = jsonFormat1(RoutingResponse.apply)
}
