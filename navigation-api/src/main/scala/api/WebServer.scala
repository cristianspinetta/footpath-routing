package api

import akka.event.Logging
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import base.conf.ApiEnvConfig
import base.{ ApiSnapshots, Contexts }
import com.typesafe.config.Config
import module.RoutingModule
import provider.GraphSupport
import scalikejdbc.config._

import scala.concurrent.{ Await, Future }

object WebServer extends App with RoutingModule with ApiEnvConfig {
  override implicit val system = Contexts.system
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  new DBs with TypesafeConfigReader with StandardTypesafeConfig with NoEnvPrefix {
    override lazy val config: Config = configuration.envConfiguration.config
  }.setupAll()

  override val logger = Logging(system, getClass)

  private val interface: String = configuration.HTTP.interface
  private val port: Int = configuration.HTTP.port

  init()
    .flatMap(_ ⇒ Http().bindAndHandle(wsRoutes, interface, port))
    .map(_ ⇒ logger.info(s"Server online at http://$interface:$port/..."))
    .recover { case exc: Throwable ⇒ logger.error(exc, s"WebServer failed to initialize.") }

  def init() = Future {
    logger.info("Application starting...")
    val graphFut = Future {
      ApiSnapshots.initialize()
      GraphSupport.getGraphSet
    } // Load graph
    Await.result(graphFut, configuration.Graph.loadingTimeout)
    logger.info("Application started successfully...")
  }

}
