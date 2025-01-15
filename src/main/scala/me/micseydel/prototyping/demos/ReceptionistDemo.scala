package me.micseydel.prototyping.demos

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.annotation.unused

// https://alvinalexander.com/scala/akka-typed-how-lookup-find-actor-receptionist/
object Brain {

  sealed trait MessageToBrain

  final case object FindTheMouth extends MessageToBrain

  private case class ListingResponse(listing: Receptionist.Listing) extends MessageToBrain

  def apply(): Behavior[MessageToBrain] = Behaviors.setup { context: ActorContext[MessageToBrain] =>
    val listingAdapter: ActorRef[Receptionist.Listing] = context.messageAdapter(Brain.ListingResponse)

    context.system.receptionist ! Receptionist.Subscribe(Mouth.MouthKey, listingAdapter)

    findTheMouth(listingAdapter)
  }

  private def findTheMouth(listingAdapter: ActorRef[Receptionist.Listing]): Behavior[MessageToBrain] =
    Behaviors.receive { case (context, message) =>
      message match {
        case FindTheMouth =>
          // this is triggered at the top level, presumably after the Registration is complete
          context.system.receptionist ! Receptionist.Find(Mouth.MouthKey, listingAdapter)
          Behaviors.same
        case ListingResponse(Mouth.MouthKey.Listing(listings)) =>
          listings.toList match {
            case List(mouth) =>
              mouth ! Mouth.SpeakText("Brain says hello to Mouth")
              sayAgain(mouth)
            case other =>
              context.log.error(s"Expected a single Mouth, did not expect $other")
              Behaviors.unhandled
          }

        case other =>
          context.log.error(s"Did not expect $other")
          Behaviors.unhandled
      }
    }


  private def sayAgain(mouth: ActorRef[Mouth.MessageToMouth]): Behavior[MessageToBrain] = Behaviors.receive {
    case (context, ListingResponse(Mouth.MouthKey.Listing(listings))) =>
      val msg = "Brain says hello to Mouth"
      context.log.info(s"Send message: $msg")
      mouth ! Mouth.SpeakText(msg)

      // just logging
      listings.toList match {
        case List(newSingleListing) if newSingleListing == mouth =>
          context.log.info("Received ListingResponse identical to before, ignoring")
        case other =>
          context.log.warn(s"Already had $mouth but received $other, ignoring")
      }

      Behaviors.same

    case (context, other) =>
      context.log.warn(s"[initialized] Not sure what to do with $other")
      Behaviors.unhandled
  }
}


object Mouth {
  sealed trait MessageToMouth

  case class SpeakText(msg: String) extends MessageToMouth

  // the service key is owned by the Registrant
  val MouthKey: ServiceKey[MessageToMouth] = ServiceKey("Mouth")

  def apply(): Behavior[MessageToMouth] = Behaviors.setup { context: ActorContext[MessageToMouth] =>
    context.system.receptionist ! Receptionist.Register(Mouth.MouthKey, context.self)

    Behaviors.receiveMessage {
      case SpeakText(msg) =>
        println(s"Mouth: got a msg: $msg")
        Behaviors.same
    }
  }
}


object Supervisor {

  sealed trait SupervisorMessage

  final case object Start extends SupervisorMessage

  def apply(): Behavior[SupervisorMessage] =
    Behaviors.setup[SupervisorMessage] { actorContext: ActorContext[SupervisorMessage] =>
      // in this case, we are not passing the mouth to the brain via construction, because we are demo'ing the
      // receptionist - normally in this case it would be more direct, but we keep things simple to show the
      // spirit of the receptionist communication; the point is that direct object passing / message passing aren't
      // aren't required in


      // the mouth's setup includes registering with the receptionist
      @unused
      val mouth: ActorRef[Mouth.MessageToMouth] = actorContext.spawn(
        Mouth(), "Mouth"
      )
      val brain: ActorRef[Brain.MessageToBrain] = actorContext.spawn(
        Brain(), "Brain"
      )

      Behaviors.receiveMessage {
        case Start =>
          println(s"Supervisor got a Start message")
          // it waits for us to tell it, since the mouth needs to register with the receptionist first
          brain ! Brain.FindTheMouth
          Behaviors.same
      }
    }
}


object ReceptionistDemo {
  def main(args: Array[String]): Unit = {
    val supervisor: ActorSystem[Supervisor.SupervisorMessage] = ActorSystem(
      Supervisor(),
      "Supervisor"
    )

    supervisor ! Supervisor.Start

    // it doesn't take long tor un
    Thread.sleep(200)
    supervisor.terminate()
  }
}
