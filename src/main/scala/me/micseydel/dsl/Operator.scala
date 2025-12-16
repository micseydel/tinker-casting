package me.micseydel.dsl

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import com.softwaremill.quicklens.ModifyPimp
import me.micseydel.actor.google.GmailActor
import me.micseydel.actor.google.GmailActor.Email
import me.micseydel.actor.perimeter.{AranetActor, HomeMonitorActor}
import me.micseydel.app.GoogleSlideUpdater
import me.micseydel.dsl.cast.{SystemWideTimeKeeper, UntrackedTimeKeeper}

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationInt

object Operator {
  sealed trait Message

  final case class SubscribeMidnight(replyTo: SpiritRef[ZonedDateTime]) extends Message

  sealed trait Register extends Message
  case class RegisterHomeMonitor(registrant: SpiritRef[HomeMonitorActor.Monitoring]) extends Register
  // FIXME: make generic, don't rely on Gmail as part of the api
  case class RegisterGmail(registrant: SpiritRef[GmailActor.Subscribe]) extends Register
  final case class RegisterGoogleSlides(slides: SpiritRef[GoogleSlideUpdater.Message]) extends Register

  private val TotalAllowedRetries: Int = 3

  final case class FetchGoogleSlides(replyTo: SpiritRef[Option[SpiritRef[GoogleSlideUpdater.Message]]]) extends Message

  sealed trait Subscribe extends Message
  case class SubscribeAranet4(subscriber: SpiritRef[AranetActor.Result]) extends Subscribe
  case class SubscribeGmail(subscriber: SpiritRef[Seq[Email]]) extends Subscribe
  private case class RetryGmailSubscription(subscriber: SpiritRef[Seq[Email]], retryNumber: Int = 1) extends Register

  // behavior

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    context.log.info("Starting operator")
    val systemWideTimeKeeper: ActorRef[SystemWideTimeKeeper.Message] = context.spawn(SystemWideTimeKeeper(), "SystemWideTimeKeeper")
    implicit val timeKeeper: ActorRef[UntrackedTimeKeeper.Message] = context.spawn(UntrackedTimeKeeper(), "UntrackedTimeKeeper")
    behavior(systemWideTimeKeeper, timeKeeper, None, None, None)
  }

  private def behavior(systemWideTimeKeeper: ActorRef[SystemWideTimeKeeper.Message], timeKeeper: ActorRef[UntrackedTimeKeeper.Message], homeMonitor: Option[SpiritRef[HomeMonitorActor.Monitoring]], gmailSubscription: Option[SpiritRef[GmailActor.Subscribe]], slides: Option[SpiritRef[GoogleSlideUpdater.Message]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    implicit val sender: Sender = Sender(context.self.path)

    message match {
      case RegisterHomeMonitor(registrant) =>
        homeMonitor match {
          case Some(oldRegistration) =>
            context.log.warn(s"Registering ${registrant.path} as HomeMonitor, replacing ${oldRegistration.path}")
          case None =>
            context.log.info(s"Registering ${registrant.path} as HomeMonitor")
        }
        behavior(systemWideTimeKeeper, timeKeeper, Some(registrant), gmailSubscription, slides)

      case RegisterGmail(subscription) =>
        gmailSubscription match {
          case Some(oldRegistration) =>
            context.log.warn(s"Registering ${subscription.path} as GmailActor, replacing ${oldRegistration.path}")
          case None =>
            context.log.info(s"Registering ${subscription.path} as GmailActor")
        }
        behavior(systemWideTimeKeeper, timeKeeper, homeMonitor, Some(subscription), slides)

      case SubscribeAranet4(subscriber) =>
        homeMonitor match {
          case Some(homeMonitorRef) =>
            context.log.info(s"Sending new subscriber ${subscriber.path} to home monitor...")
            homeMonitorRef !!! HomeMonitorActor.SubscribeAranet4(subscriber)
          case None =>
            context.log.warn(s"New subscriber ${subscriber.path} to home monitor but no registered home monitor")
        }
        Behaviors.same

      case SubscribeGmail(subscriber) =>
        gmailSubscription match {
          case Some(gmailActorRef) =>
            context.log.info(s"Sending new subscriber ${subscriber.path} to GmailActor...")
            gmailActorRef !!! GmailActor.Subscribe(subscriber)
          case None =>
            context.log.info(s"New subscriber ${subscriber.path} to gmail but no registered GmailActor - fine on startup, but may be an issue if it's happening more than then")
            val delay = 5.seconds // FIXME
            val retryMessage = RetryGmailSubscription(subscriber)
            timeKeeper ! UntrackedTimeKeeper.RemindMeIn(delay, context.self, retryMessage, None)
        }
        Behaviors.same

      case retry@RetryGmailSubscription(subscriber, retryCount) =>
        if (retryCount > TotalAllowedRetries) {
          context.log.warn(s"Ignoring $subscriber after $retryCount")
        } else {
          gmailSubscription match {
            case Some(gmail) =>
              context.log.info(s"Subscribing $subscriber after $retryCount retries")
              gmail !!! GmailActor.Subscribe(subscriber)
            case None =>
              val delay = 5.seconds // FIXME
              context.log.info(s"Subscription for $subscriber still retrying after $retryCount retries, up to $TotalAllowedRetries")
              timeKeeper ! UntrackedTimeKeeper.RemindMeIn(delay, context.self, retry.modify(_.retryNumber).using(_ + 1), None)
          }
        }
        Behaviors.same

      case SubscribeMidnight(replyTo) =>
        systemWideTimeKeeper ! SystemWideTimeKeeper.SubscribeMidnight(replyTo)
        Behaviors.same

      case RegisterGoogleSlides(slides) =>
        behavior(systemWideTimeKeeper, timeKeeper, homeMonitor, gmailSubscription, Some(slides))

      case FetchGoogleSlides(replyTo) =>
        if (slides.isEmpty) context.log.warn(s"$replyTo requested slides, but we don't have slides yet!")
        replyTo !!! slides
        Behaviors.same
    }
  }
}
