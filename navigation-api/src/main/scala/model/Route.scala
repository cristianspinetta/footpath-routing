package model

import akka.http.scaladsl.unmarshalling.Unmarshaller
import base.CaseObjectSerializationSupport
import mapdomain.graph.Coordinate
import spray.json.DefaultJsonProtocol

case class Route(path: List[Path])

case class Path(path: List[Coordinate], description: PathDescription)

case class PathDescription(`type`: PathType, from: String, to: String)

sealed trait PathType
case object WalkPath extends PathType
case object BusPath extends PathType

object PathType {
  val keyMap: Map[String, PathType] = Map(
    "walk" -> WalkPath,
    "bus" -> BusPath)
}

sealed trait TypeRouting
case object StreetRouting extends TypeRouting
case object SidewalkRouting extends TypeRouting

object TypeRouting {
  val keyMap: Map[String, TypeRouting] = Map(
    "street" -> StreetRouting,
    "sidewalk" -> SidewalkRouting) withDefaultValue SidewalkRouting
}

trait RouteFormatter extends DefaultJsonProtocol with CaseObjectSerializationSupport with ModelFormatter {

  //  implicit val SidewalkRoutingFormat = caseObjectJsonFormat[SidewalkRouting.type](SidewalkRouting)
  implicit val TypeRoutingFormat = caseObjectJsonFormat[TypeRouting](StreetRouting, SidewalkRouting)
  implicit val TypeRoutingUnmarshaller = Unmarshaller.strict[String, TypeRouting](TypeRouting.keyMap)
  //  implicit val SidewalkRoutingUnmarshaller = Unmarshaller.strict[String, SidewalkRouting.type](Map(TypeRouting.keyMap.find(_._2 == SidewalkRouting).get._1 -> SidewalkRouting))

  implicit val PathTypeFormat = caseObjectJsonFormat[PathType](WalkPath, BusPath)
  implicit val PathDescriptionFormat = jsonFormat3(PathDescription.apply)
  implicit val PathFormat = jsonFormat2(Path.apply)
  implicit val RouteFormat = jsonFormat1(Route.apply)

}
