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
import model._
import module.{ GraphSupport, MapGeneratorModule, MapModule, RoutingModule }
import scalikejdbc.ConnectionPool
import spray.json._

import scala.concurrent.{ Await, ExecutionContextExecutor, Future }

final case class RoutingRequest(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double, routingType: TypeRouting = SidewalkRouting)
object RoutingRequest {
  def applyWithDefault(fromLng: Double, fromLat: Double, toLng: Double, toLat: Double): RoutingRequest = new RoutingRequest(fromLng, fromLat, toLng, toLat)
}
final case class EdgeRequest(edgeType: EdgeType, radius: Double, lat: Double, lng: Double)
final case class RampRequest(lat: Double, lng: Double, radius: Double)
final case class StreetResponse(streets: Iterable[Street])
final case class SidewalkResponse(sidewalks: Iterable[Sidewalk])
final case class EdgeResponse(edges: Iterable[Edge])
final case class RampResponse(ramps: Vector[Ramp])

trait DirectionService extends ApiEnvConfig {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  import Protocol._

  val logger: LoggingAdapter

  def init() = Future {
    val graphFut = Future(GraphSupport.getGraphSet) // Load graph
    Await.result(graphFut, configuration.Graph.loadingTimeout)
  }

  val routes = CorsDirectives.cors() {
    logRequest("routing-request", akka.event.Logging.InfoLevel) {
      get {
        path("route") {
          parameters('fromLng.as[Double], 'fromLat.as[Double], 'toLng.as[Double], 'toLat.as[Double] /*, 'routingType.as[TypeRouting] ? SidewalkRouting*/ ).as(RoutingRequest.applyWithDefault _) { routingRequest ⇒
            val response: Future[ToResponseMarshallable] = Future.successful {
              RoutingModule.routing(
                coordinateFrom = Coordinate(routingRequest.fromLat, routingRequest.fromLng),
                coordinateTo = Coordinate(routingRequest.toLat, routingRequest.toLng),
                routingType = routingRequest.routingType).get
            }
            complete(response)
          }
        } ~
          path("health-check") {
            val dbStatus: Either[String, String] = if (ConnectionPool.isInitialized()) Right("DB connected")
            else Left("DB is not connected")
            val graphStatus: Either[String, String] = if (GraphSupport.graphLoaded) Right("Graph loaded")
            else Left(s"Graph has not loaded yet. Status: ${GraphSupport.status}")
            val responseBody: JsValue = Map(
              "status" -> Map(
                "DB" -> dbStatus.merge,
                "Graph" -> graphStatus.merge)).toJson
            val responseStatus = (dbStatus, graphStatus) match {
              case (Right(_), Right(_)) ⇒ OK
              case _                    ⇒ ServiceUnavailable
            }
            complete(responseStatus -> responseBody)
          } ~
          pathPrefix("map") {
            path("edges") {
              parameters('edgeType.as[EdgeType], 'radius ? 1.0D, 'lat.as[Double], 'lng.as[Double]).as(EdgeRequest) { edgeRequest ⇒
                val response: Future[ToResponseMarshallable] = Future.successful {
                  val list = MapModule.edges(edgeRequest.edgeType, Coordinate(edgeRequest.lat, edgeRequest.lng), edgeRequest.radius).get
                  EdgeResponse(list.toList)
                }
                complete(response)
              }
            } ~
              path("ramps") {
                parameters('lat.as[Double], 'lng.as[Double], 'radius ? 1.0D).as(RampRequest) { rampRequest: RampRequest ⇒
                  val response: Future[ToResponseMarshallable] = Future.successful {
                    val list = MapModule.ramps(Coordinate(rampRequest.lat, rampRequest.lng), rampRequest.radius).get
                    RampResponse(list)
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

}
