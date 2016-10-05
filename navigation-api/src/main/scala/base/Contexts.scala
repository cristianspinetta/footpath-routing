package base

import akka.actor.ActorSystem

object Contexts {
  val system = ActorSystem()
  val routingExecutionContext = system.dispatchers.lookup("execution-contexts.routing")
}
