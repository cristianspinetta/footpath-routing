package api

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import base.Contexts
import com.typesafe.config.Config
import base.conf.ApiEnvConfig
import scalikejdbc.config._
import module.RoutingModule

import scala.util.{ Failure, Success }

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

  val bindingFuture = Http().bindAndHandle(routes, interface, port)

  bindingFuture foreach { binder ⇒

    init().onComplete {
      case Success(_)   ⇒ logger.info(s"WebServer initialized successfully.")
      case Failure(exc) ⇒ logger.error(exc, s"WebServer failed to initialize.")
    }

    logger.info(s"Server online at http://$interface:$port/...")

    //    StdIn.readLine() // let it run until user presses return
    //
    //    binder.unbind() // trigger unbinding from the port
    //      .onComplete(_ ⇒ system.terminate()) // and shutdown when done
  }

}
