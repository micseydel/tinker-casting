package me.micseydel.app

import akka.actor.typed.ActorSystem
import me.micseydel.actor.kitties.LitterBoxReportActor
import me.micseydel.model._
//import me.micseydel.vault.LitterBoxReportActor.Correction

import java.time.ZonedDateTime
import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object AppCommandLineInterface {

//  def cliLoop(system: ActorSystem[Correction]): Unit = while (true) try {
//    println("To issue a correction, (d)elete or (o)verwrite; or (e)xit: ")
//    StdIn.readLine() match {
//      case "d" =>
//        val key = TEMPgetDateFromStdin()
//        system ! LitterBoxReportActor.Deletion(key)
//      case "o" =>
//        val event: LitterSiftedEvent = getLitterSiftEventFromStdin()
//        system ! LitterBoxReportActor.Overwrite(event)
//
//      case _ =>
//        println()
//    }
//  } catch {
//    case exception: Throwable =>
//      println("Something went wrong")
//      exception.printStackTrace()
//  }

  //noinspection AccessorLikeMethodIsEmptyParen
  @tailrec
  private def getDateFromStdin(): ZonedDateTime = {
    println(s"Please provide a ZonedDateTime like, e.g. ${ZonedDateTime.now()}: ")
    Try {
      ZonedDateTime.parse(StdIn.readLine())
    } match {
      case Success(value) =>
        value
      case Failure(exception) =>
        print(s"Failed to get ZonedDateTime: $exception")
        exception.printStackTrace()
        getDateFromStdin()
    }
  }

  //noinspection AccessorLikeMethodIsEmptyParen
  @tailrec
  private def TEMPgetDateFromStdin(): String = {
    println(s"Please provide a key, e.g. ${ZonedDateTime.now()}: ")
    val userIn = StdIn.readLine()
    Try {
      ZonedDateTime.parse(userIn)
    } match {
      case Success(_) =>
        userIn
      case Failure(exception) =>
        print(s"Failed to get ZonedDateTime (this code may need to be modified to support compound keys): $exception")
        exception.printStackTrace()
        TEMPgetDateFromStdin()
    }
  }

  //noinspection AccessorLikeMethodIsEmptyParen
  private def getLitterSiftEventFromStdin(): LitterSiftedEvent = {
    val when = getDateFromStdin()

    println("Start with f/b/r and then just 1s and 2s, e.g  f112 is two pees and one poop in the front litter box: ")
    val (litterBoxChoice, siftedContents) = StdIn.readLine().toList match {
      case LitterBoxChoice(choice) :: eliminationTypes =>
        (
          choice,
          SiftedContents(eliminationTypes.map {
            case '1' => Urination
            case '2' => Defecation
          })
        )
      case other =>
        throw new RuntimeException(s"StdIn not as expected: $other")
    }

    LitterSiftedEvent(when, litterBoxChoice, siftedContents)
  }
}
