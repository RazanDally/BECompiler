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
import hmloc.utils.TestHelperConsts._
import os.Path

import scala.math
object MainApp {

  /**
   * check if the string contains a letter or digit character, by checking ascii of
   * each character whether it is 0-9 a-z or A-Z
   * @param str the string to check
   * @return true if the string contains a letter or digit character
   */
    def containsLetterOrDigit(str: String): Boolean = {
      str.exists(c => c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
    }

  /**
   * a function that colors the text underlined by Carets (by setting span tags)
   * @param output the output to color
   * @return the colored output
   */
  def colorCarets(output: String): String = {
    val lines = output.split("\n")
    var listOfNewLines:List[String] = List()

    var lastWasCaret = false
    var i = 0
    while (i  < lines.length - 1) {
      val topLineList: List[Char] = lines(i).toList
      val bottomLineList: List[Char] = lines(i + 1).toList

      //if bottom line does not includes any numbers or letter, the we can go, and includes carets, then we can go
      if (!containsLetterOrDigit(bottomLineList.mkString) && bottomLineList.contains('^')
        && lines(i + 1).startsWith(outputMarker)) {

        //this is to fix the issue where ^ are one index to the right over the topLine
        //this only happens when the top line includes a line number
        var skipFirst = lines(i).contains("- l.")

        //advance the loop by an extra line as we are going to add the carets
        i += 1


        lastWasCaret = true
        var newTopLine = ""
        var newBottomLine = ""
        var inCaret = false
        val minLength = math.min(topLineList.length, bottomLineList.length)
        for (j <- 0 until minLength) {
          if (!inCaret && bottomLineList(j) == '^') {
            inCaret = true
            newTopLine += "<span class=\"caret_underlined\">"
            newBottomLine += "<span class=\"carets\">"
          }
          newTopLine += topLineList(j)
          if(skipFirst && bottomLineList(j) == ' ') {
            skipFirst = false
          }
          else newBottomLine += bottomLineList(j)

          if (inCaret && bottomLineList(j) != '^') {
            inCaret = false
            newTopLine += "</span>"
            newBottomLine += "</span>"
          }
        }
        if (inCaret) {
          newTopLine += "</span>"
          newBottomLine += "</span>"
        }
        //if the top line is longer than the bottom line, then we need to add the rest of the top line
        if (topLineList.length > bottomLineList.length) {
          for (j <- bottomLineList.length until topLineList.length) {
            newTopLine += topLineList(j)
          }
        }
        //if the bottom line is longer than the top line, then we need to add the rest of the bottom line
        if (bottomLineList.length > topLineList.length) {
          for (j <- topLineList.length until bottomLineList.length) {
            newBottomLine += bottomLineList(j)
          }
        }

        //add the new lines to the list
        listOfNewLines = listOfNewLines.appended(newTopLine)
        listOfNewLines = listOfNewLines.appended(newBottomLine)
      }
      else
      {
        lastWasCaret = false
        var newList: List[String] = List().appendedAll(listOfNewLines).appended(lines(i))
        listOfNewLines = newList

      }
      //advance the loop
      i += 1
    }

    //if the last line was not a caret, then we need to add it to the list, as we wouldve skipped it
    if(!lastWasCaret) {
      listOfNewLines = listOfNewLines.appended(lines(i))
    }

    //return all lines with \n between them
    listOfNewLines.mkString("\n")
  }

  def addLineButtons(output:String) : String = {
    //using regex, looks for sections that look like '- l.number' or l.number:number
    // and change them to be surrounded by <button  class=\\\"line_number\\\"> and </button>
    val lineNumRegex = """(-? l\.(\-)?\d+)|l\.(\-)?\d+:\d+""".r
    val newOutput = lineNumRegex.replaceAllIn(output, m => s"<button  class=\\\"line_number\\\">${m.group(0)}</button>")
    newOutput
  }

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.executionContext

    // Define the route that takes a number as a value in the URL
    val routes =
      pathPrefix("runTest") {
        path(IntNumber) { number =>
          get {
            val output = runTestFileSurveyHard(number)
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"<code><pre> $output </pre></code>"))
          }
        } ~
          pathEndOrSingleSlash {
            get {
              // run the test
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Write runTest/ a number from 1 - 3 in link"))
            }
          }
      } ~
      pathPrefix("runPath") {
        path(Segment) { mypath =>
          get {
            //change $ in mypath to / in path
            val path = os.Path.apply(mypath.replace('$', '/'))

            val output = addLineButtons(colorCarets(runGivenPath(path)))

            // run the test
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"<code><pre style=\"text-wrap:pretty\"> $output </pre></code>"))
          }
        }
      }

    val futureBinding = Http().newServerAt("localhost", 8080).bind(routes)
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