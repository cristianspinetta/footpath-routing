package service

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ ExceptionHandler, MethodRejection, RejectionHandler }
import akka.stream.Materializer
import ch.megard.akka.http.cors.CorsDirectives
import conf.ApiEnvConfig
import mapdomain.graph.Coordinate
import mapdomain.sidewalk.Ramp
import model.{ Edge, EdgeType, Sidewalk, Street }
import module.{ MapGeneratorModule, MapModule, RoutingModule }

import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.util.{ Failure ⇒ TFailure, Success ⇒ TSuccess }

final case class RoutingRequest(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double, routingType: String) {
  val routingTypeO: TypeRouting = TypeRouting(routingType)
}
final case class EdgeRequest(edgeType: String, radius: Double, lat: Double, lng: Double)
final case class RampRequest(lat: Double, lng: Double, radius: Double)
final case class RoutingResponse(path: Iterable[Coordinate])
final case class StreetResponse(streets: Iterable[Street])
final case class SidewalkResponse(sidewalks: Iterable[Sidewalk])
final case class EdgeResponse(edges: Iterable[Edge])
final case class RampResponse(ramps: Vector[Ramp])

sealed trait TypeRouting { val key: String }
case object StreetRouting extends TypeRouting { val key = "street" }
case object SidewalkRouting extends TypeRouting { val key = "sidewalk" }

object TypeRouting {
  def apply(typeRouting: String): TypeRouting = typeRouting match {
    case StreetRouting.key ⇒ StreetRouting
    case _                 ⇒ SidewalkRouting
  }
}

trait DirectionService extends ApiEnvConfig {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  import Protocol._

  val logger: LoggingAdapter

  implicit def exceptionHandler = ExceptionHandler {
    case exc: Throwable ⇒
      extractUri { uri ⇒
        logger.error(exc, s"Request to $uri could not be handled normally")
        complete(HttpResponse(InternalServerError, entity = exc.getMessage))
      }
  }

  implicit def rejectionHandler = RejectionHandler.newBuilder()
    .handleAll[MethodRejection] { methodRejections ⇒
      val names = methodRejections.map(_.supported.name)
      complete((MethodNotAllowed, s"Can't do that! Supported: ${names mkString " or "}!"))
    }
    .handleNotFound { complete((NotFound, "Not here!")) }
    .result()

  val routes = CorsDirectives.cors() {
    logRequest("routing-request", akka.event.Logging.InfoLevel) {
      get {
        path("directions") {
          parameters('fromLng.as[Double], 'fromLat.as[Double], 'toLng.as[Double], 'toLat.as[Double],
            'routingType ? "street").as(RoutingRequest) { routingRequest ⇒
              val response: Future[ToResponseMarshallable] = Future.successful {
                RoutingModule.routing(
                  coordinateFrom = Coordinate(routingRequest.fromLat, routingRequest.fromLng),
                  coordinateTo = Coordinate(routingRequest.toLat, routingRequest.toLng),
                  routingType = routingRequest.routingTypeO) get
              }
              complete(response)
            }
        } ~
          pathPrefix("map") {
            path("edges") {
              parameters('edgeType.as[String], 'radius ? 1.0D, 'lat.as[Double], 'lng.as[Double]).as(EdgeRequest) { edgeRequest ⇒
                val response: Future[ToResponseMarshallable] = Future.successful {
                  MapModule.edges(EdgeType(edgeRequest.edgeType), Coordinate(edgeRequest.lat, edgeRequest.lng), edgeRequest.radius) get
                }
                complete(response)
              }
            } ~
              path("ramps") {
                parameters('lat.as[Double], 'lng.as[Double], 'radius ? 1.0D).as(RampRequest) { rampRequest: RampRequest ⇒
                  val response: Future[ToResponseMarshallable] = Future.successful {
                    MapModule.ramps(Coordinate(rampRequest.lat, rampRequest.lng), rampRequest.radius) get
                  }
                  complete(response)
                }
              }
          } ~
          pathPrefix("private") {
            pathPrefix("create") {
              path("street") {
                val response: Future[ToResponseMarshallable] = Future.successful {
                  MapGeneratorModule.createStreets() map (_ ⇒ "") get
                }
                complete(response)
              } ~
                path("sidewalk") {
                  val response: Future[ToResponseMarshallable] = Future.successful {
                    MapGeneratorModule.createSidewalks() map (_ ⇒ "") get
                  }
                  complete(response)
                }
            }
          }
      }
    }
  }

}
