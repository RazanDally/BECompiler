package hmloc

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route.seal

import scala.util.Failure
import scala.util.Success

import hmloc.DFRunner._
object MainApp {


  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext

    // Define the route that takes a number as a value in the URL
    val route: Route =
      pathPrefix("runTest") {
        path(IntNumber) { number =>
          get {
            val output = runAllFiles(number)
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"<code><pre> $output </pre></code>"))
          }
        } ~
          pathEndOrSingleSlash {
            get {
              // run the test
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Write runTest/ a number from 1 - 3 in link"))
            }
          }
      }

    val futureBinding = Http().newServerAt("localhost", 8080).bind(route)
    futureBinding.onComplete {
      case Success(binding) =>
        val address = binding.localAddress
        system.log.info("Server online at http://{}:{}/", address.getHostString, address.getPort)
      case Failure(ex) =>
        system.log.error("Failed to bind HTTP endpoint, terminating system", ex)
        system.terminate()
    }
  }
}