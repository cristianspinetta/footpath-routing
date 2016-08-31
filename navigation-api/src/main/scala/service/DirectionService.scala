package service

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import ch.megard.akka.http.cors.CorsDirectives
import conf.ApiEnvConfig
import mapdomain.graph.Coordinate
import mapdomain.sidewalk.Ramp
import model.{ Edge, EdgeType, Sidewalk, Street }
import module.{ MapModule, RoutingModule }

import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.util.{ Failure ⇒ TFailure, Success ⇒ TSuccess }

case class RoutingRequest(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double, routingType: String) {
  val routingTypeO: TypeRouting = TypeRouting(routingType)
}
case class EdgeRequest(edgeType: String)
case class RoutingResponse(path: Iterable[Coordinate])
case class StreetResponse(streets: Iterable[Street])
case class SidewalkResponse(sidewalks: Iterable[Sidewalk])
case class EdgeResponse(edges: Iterable[Edge])
case class RampResponse(ramps: Vector[Ramp])

trait TypeRouting
case object StreetRouting extends TypeRouting
case object SidewalkRouting extends TypeRouting

object TypeRouting {
  def apply(typeRouting: String): TypeRouting = typeRouting match {
    case "street" => StreetRouting
    case "sidewalk" => SidewalkRouting
    case _ => SidewalkRouting
  }
}

trait DirectionService extends ApiEnvConfig {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  import Protocol._

  val logger: LoggingAdapter

  val routes = CorsDirectives.cors() {
    logRequestResult("routing-request") {
      get {
        path("directions") {
          parameters('fromLng.as[Double], 'fromLat.as[Double], 'toLng.as[Double], 'toLat.as[Double], 'routingType ? "street").as(RoutingRequest) { routingRequest ⇒
            val response: Future[ToResponseMarshallable] = Future.successful {
              RoutingModule.routing(Coordinate(routingRequest.fromLat, routingRequest.fromLng), Coordinate(routingRequest.toLat, routingRequest.toLng), routingRequest.routingTypeO) match {
                case TSuccess(list)           ⇒ RoutingResponse(list.map(coor ⇒ Coordinate(coor.latitude, coor.longitude)))
                case TFailure(exc: Throwable) ⇒ BadRequest -> exc.getMessage
              }
            }
            complete(response)
          }
        } ~
          path("ramps") {
            val response: Future[ToResponseMarshallable] = Future.successful {
              RoutingModule.ramps match {
                case TSuccess(list)           ⇒ RampResponse(list)
                case TFailure(exc: Throwable) ⇒ BadRequest -> exc.getMessage
              }
            }
            complete(response)
          } ~
          pathPrefix("map") {
            path("streets") {
              val response: Future[ToResponseMarshallable] = Future.successful {
                MapModule.streets match {
                  case TSuccess(list) ⇒ StreetResponse(list)
                  case TFailure(exc: Throwable) ⇒
                    logger.error(exc, s"Failed")
                    BadRequest -> exc.getMessage
                }
              }
              complete(response)
            } ~
              path("sidewalks") {
                val response: Future[ToResponseMarshallable] = Future.successful {
                  MapModule.sidewalks match {
                    case TSuccess(list)           ⇒ SidewalkResponse(list.toList)
                    case TFailure(exc: Throwable) ⇒ BadRequest -> exc.getMessage
                  }
                }
                complete(response)
              } ~
              path("edges") {
                parameters('edgeType.as[String]).as(EdgeRequest) { routingRequest ⇒
                  val response: Future[ToResponseMarshallable] = Future.successful {
                    MapModule.edges(EdgeType(routingRequest.edgeType)) match {
                      case TSuccess(list)           ⇒ EdgeResponse(list.toList)
                      case TFailure(exc: Throwable) ⇒ BadRequest -> exc.getMessage
                    }
                  }
                  complete(response)
                }
              }
          }
      }
    }
  }

}
