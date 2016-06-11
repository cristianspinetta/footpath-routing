package api

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import service.DirectionService

import scala.io.StdIn

object WebServer extends App with DirectionService {
  override implicit val system = ActorSystem()
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  override val config = ConfigFactory.load()
  override val logger = Logging(system, getClass)

  private val interface: String = config.getString("http.interface")
  private val port: Int = config.getInt("http.port")

  val bindingFuture = Http().bindAndHandle(routes, interface, port)

  println(s"Server online at http://$interface:$port/\nPress RETURN to stop...")

  StdIn.readLine() // let it run until user presses return

  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.terminate()) // and shutdown when done
}
