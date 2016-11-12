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
  implicit val EdgeTypeFormat = caseObjectJsonFormat[EdgeType](StreetEdgeType, SidewalkEdgeType, StreetCrossingEdgeType)
  implicit val VertexTypeFormat = caseObjectJsonFormat[VertexType](StreetVertexType, SidewalkVertexType)
  implicit val ReportableElementTypeFormat = caseObjectJsonFormat[ReportableElementType](RAMP, SIDEWALK)
  implicit val RampFormat = jsonFormat4(Ramp.apply)
  implicit val EdgeFormat = jsonFormat4(Edge.apply)
  implicit val VertexFormat = jsonFormat3(Vertex.apply)
  implicit val MapContainerFormat = jsonFormat2(MapContainer.apply)
  implicit val StreetFormat = jsonFormat3(Street.apply)
  implicit val SidewalkFormat = jsonFormat2(Sidewalk.apply)
  implicit val ReportableElementFormat = jsonFormat6(ReportableElement.apply)
  implicit val StopFormat = jsonFormat4(Stop.apply)
  implicit val PublicTransportPathFormat = jsonFormat4(PublicTransportPath)
  implicit val StopCombinationFormat = jsonFormat6(PTCombination.apply)

  implicit val EdgeTypeUnmarshaller = Unmarshaller.strict[String, EdgeType](EdgeTypeFormat.mapping)
}
