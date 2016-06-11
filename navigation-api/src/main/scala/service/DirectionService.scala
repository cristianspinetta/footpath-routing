package service

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import conf.EnvConfig
import module.RoutingModule
import spray.json.DefaultJsonProtocol

import scala.concurrent.{ ExecutionContextExecutor, Future }
import scala.util.Random
import scala.util.{ Success ⇒ TSuccess, Failure ⇒ TFailure }

case class RoutingRequest(from: Double, to: Double)
case class RoutingResponse(path: List[Coordinate])

case class Coordinate(latitude: Double, longitude: Double)

trait Protocols extends DefaultJsonProtocol {
  implicit val coordinateFormat = jsonFormat2(Coordinate.apply)
  implicit val RoutingRequestFormat = jsonFormat2(RoutingRequest.apply)
  implicit val RoutingResponseFormat = jsonFormat1(RoutingResponse.apply)
}

trait DirectionService extends Protocols with EnvConfig {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  val logger: LoggingAdapter

  def fetchDirections(routingRequest: RoutingRequest): Future[Either[String, RoutingResponse]] = {
    Future.successful {
      RoutingModule.routing(routingRequest.from.toInt, routingRequest.to.toInt) match {
        case TSuccess(list)           ⇒ Right(RoutingResponse(list.map(coor => Coordinate(coor.latitude, coor.longitude))))
        case TFailure(exc: Throwable) ⇒ Left(exc.getMessage)
      }
    }
  }

  val routes = logRequestResult("routing-request") {
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
    }
  }

}
