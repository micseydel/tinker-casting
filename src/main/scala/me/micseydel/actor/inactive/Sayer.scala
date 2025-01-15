package me.micseydel.actor.inactive

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.sys.process._

object Sayer {
  sealed trait Command
  case class Say(string: String) extends Command

  // FIXME: should centralize this or at least de-dupe cat pee requests
  def apply(announceOnInit: Boolean = true): Behavior[Command] =
    Behaviors.setup { context =>
      context.log.info(s"Initializing Sayer, announceOnInit=$announceOnInit")
      if (announceOnInit) {
        context.self ! Say("Sayer initialized")
      }

      Behaviors.receive[Command] { (context, message) =>
        message match {
          case Say(string) =>
            context.log.info(s"About to say: $string")
//            speak(string)
            context.log.warn("Sayer did not speak() because it is disabled")
            Behaviors.same
        }
      }
    }

  // demo
//  def main(args: Array[String]): Unit = {
//    speak("Hello, World!")
//    "say Simply, hello".!
//  }

  private def speak(text: String): Unit = {
    // this is the macOS `say` command
    val command = s"say '${text.replace("'", "")}'"
    command.!
  }
}
