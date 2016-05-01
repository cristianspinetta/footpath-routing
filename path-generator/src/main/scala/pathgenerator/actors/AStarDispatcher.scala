package pathgenerator.actors

import akka.actor.{ Actor, Props }
import pathgenerator.graph.Vertex

object AStarDispatcher {

  case class AStarRequest(vertexStart: Vertex, vertexTarget: Vertex)

  def props = Props(classOf[AStarDispatcher])
}

class AStarDispatcher extends Actor {
  import AStarDispatcher._

  override def receive: Receive = handleRequest

  def handleRequest: Receive = {
    case AStarRequest(vertexStart, vertexTarget) ⇒
  }
}
