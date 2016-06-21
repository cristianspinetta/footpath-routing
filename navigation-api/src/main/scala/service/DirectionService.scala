package service

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import ch.megard.akka.http.cors.CorsDirectives
import conf.EnvConfig
import mapgenerator.source.osm.graph.Ramp
import module.RoutingModule
import pathgenerator.graph.Coordinate

import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.util.{ Failure ⇒ TFailure, Success ⇒ TSuccess }

case class RoutingRequest(from: Double, to: Double)
case class RoutingResponse(path: List[Coordinate])
case class RampResponse(ramps: Vector[Ramp])

trait DirectionService extends EnvConfig {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  import Protocol._

  val logger: LoggingAdapter

  def fetchDirections(routingRequest: RoutingRequest): Future[Either[String, RoutingResponse]] = {
    Future.successful {
      RoutingModule.routing(routingRequest.from.toLong, routingRequest.to.toLong) match {
        case TSuccess(list)           ⇒ Right(RoutingResponse(list.map(coor ⇒ Coordinate(coor.latitude, coor.longitude))))
        case TFailure(exc: Throwable) ⇒ Left(exc.getMessage)
      }
    }
  }

  def fetchRamps: Future[Either[String, RampResponse]] = {
    Future.successful {
      RoutingModule.ramps match {
        case TSuccess(list)           ⇒ Right(RampResponse(list))
        case TFailure(exc: Throwable) ⇒ Left(exc.getMessage)
      }
    }
  }

  val routes = CorsDirectives.cors() {
    logRequestResult("routing-request") {
      path("directions") {
        get {
          parameters('from.as[Double], 'to.as[Double]).as(RoutingRequest) { routingRequest ⇒
            val response: Future[ToResponseMarshallable] = fetchDirections(routingRequest).map {
              case Right(routingResponse) ⇒ routingResponse
              case Left(errorMessage)     ⇒ BadRequest -> errorMessage
            }
            complete(response)
          }
        }
      } ~
        path("ramps") {
          get {
            val response: Future[ToResponseMarshallable] = fetchRamps.map {
              case Right(rampsResponse) ⇒ rampsResponse
              case Left(errorMessage)   ⇒ BadRequest -> errorMessage
            }
            complete(response)
          }
        }
    }
  }

}
