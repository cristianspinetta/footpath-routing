package module

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.model.{ HttpEntity, HttpResponse, StatusCode }
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.LoggingMagnet
import akka.http.scaladsl.server.{ ExceptionHandler, MethodRejection, RejectionHandler, RouteResult }
import akka.stream.Materializer
import base.ApiSnapshots
import base.conf.ApiEnvConfig
import cats.data.Xor
import ch.megard.akka.http.cors.CorsDirectives
import mapdomain.graph.Coordinate
import model._
import provider.GraphSupport
import scalikejdbc.ConnectionPool
import searching.SearchRoutingErrors._
import service.{ MapGeneratorService, MapService, RoutingService }
import spray.json._

import scala.concurrent.{ Await, ExecutionContextExecutor, Future }

trait RoutingModule extends ApiEnvConfig {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  import Protocol._

  val logger: LoggingAdapter

  def init() = Future {
    logger.info("Application starting...")
    val graphFut = Future {
      ApiSnapshots.initialize()
      GraphSupport.getGraphSet
    } // Load graph
    Await.result(graphFut, configuration.Graph.loadingTimeout)
    logger.info("Application started successfully...")
  }

  def requestMethodAndResponseStatusAsInfo(result: RouteResult): Unit = {
    def message(status: StatusCode, body: Option[String] = None): String = s"Result: $status${body.map(b ⇒ s" - $b").getOrElse("")}"
    def truncatedString(string: String): String = {
      val maxSize = 100
      if (string.length > maxSize)
        string.take(maxSize).toString + s"... and [${string.length - maxSize}] more."
      else
        super.toString
    }
    result match {
      case RouteResult.Complete(res) ⇒
        res.entity match {
          case HttpEntity.Strict(contentType, data) if res.status == OK ⇒ logger.info(message(res.status, Some(truncatedString(data.utf8String))))
          case HttpEntity.Strict(contentType, data) ⇒ logger.info(message(res.status, Some(data.utf8String)))
          case _ ⇒ logger.info(message(res.status))
        }
      case _ ⇒ // no log entries for rejections
    }
  }

  val wsRoutes = CorsDirectives.cors() {
    logResult(LoggingMagnet(_ ⇒ requestMethodAndResponseStatusAsInfo)) {
      logRequest("routing-request", akka.event.Logging.InfoLevel) {
        get {
          path("route") {
            parameters('fromLng.as[Double], 'fromLat.as[Double], 'toLng.as[Double], 'toLat.as[Double]).as(RoutingRequest.applyWithDefault _) { routingRequest ⇒
              val routeResult = RoutingService.searchRoute(
                from = Coordinate(routingRequest.fromLat, routingRequest.fromLng),
                to = Coordinate(routingRequest.toLat, routingRequest.toLng)).value.map[ToResponseMarshallable] {
                  case Xor.Right(routes) ⇒ OK -> routes
                  case Xor.Left(NoStops) ⇒ BadRequest -> "Could not find stops."
                  case Xor.Left(NoPath)  ⇒ BadRequest -> "Could not find a path."
                }
              complete(routeResult)
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
                    val mapContainer = MapService.edges(edgeRequest.edgeType, Coordinate(edgeRequest.lat, edgeRequest.lng), edgeRequest.radius).get
                    EdgeResponse(mapContainer.edges, mapContainer.vertices)
                  }
                  complete(response)
                }
              } ~
                path("ramps") {
                  parameters('lat.as[Double], 'lng.as[Double], 'radius ? 1.0D).as(RampRequest) { rampRequest: RampRequest ⇒
                    val response: Future[ToResponseMarshallable] = Future.successful {
                      val list = MapService.ramps(Coordinate(rampRequest.lat, rampRequest.lng), rampRequest.radius).get
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
                    MapGeneratorService.createStreets() map (_ ⇒ "") get
                  }
                  complete(response)
                } ~
                  path("sidewalk") {
                    val response: Future[ToResponseMarshallable] = Future.successful {
                      MapGeneratorService.createSidewalks() map (_ ⇒ "") get
                    }
                    complete(response)
                  } ~
                  path("ramp") {
                    val response: Future[ToResponseMarshallable] = Future.successful {
                      MapGeneratorService.createRamps() map (_ ⇒ "") get
                    }
                    complete(response)
                  }
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
