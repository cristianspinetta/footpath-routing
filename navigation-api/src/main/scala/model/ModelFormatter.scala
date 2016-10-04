package model

import akka.http.scaladsl.unmarshalling.Unmarshaller
import base.CaseObjectSerializationSupport
import mapdomain.graph.Coordinate
import mapdomain.sidewalk.Ramp
import spray.json.{ DefaultJsonProtocol, DeserializationException, JsNumber, JsObject, JsValue, RootJsonFormat }

trait ModelFormatter extends DefaultJsonProtocol with CaseObjectSerializationSupport {

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
  implicit val EdgeTypeFormat = caseObjectJsonFormat[EdgeType](StreetEdgeType, SidewalkEdgeType, WayEdgeType, WayAreaEdgeType)
  implicit val RampFormat = jsonFormat4(Ramp.apply)
  implicit val EdgeFormat = jsonFormat3(Edge.apply)
  implicit val VertexFormat = jsonFormat2(Vertex.apply)
  implicit val MapContainerFormat = jsonFormat2(MapContainer.apply)
  implicit val StreetFormat = jsonFormat3(Street.apply)
  implicit val SidewalkFormat = jsonFormat2(Sidewalk.apply)

  implicit val EdgeTypeUnmarshaller = Unmarshaller.strict[String, EdgeType](EdgeType.keyMap)
}
