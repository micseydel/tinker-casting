package me.micseydel.dsl.tinkerer

import akka.actor.typed.Scheduler
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import me.micseydel.actor.RasaActor
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler.ListenerAcknowledgement
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.model.{NotedTranscription, RasaResult}
import me.micseydel.util.StringImplicits.RichString
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.Await
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success}

object TinkerListener {
  sealed trait Message

  case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message

  sealed trait ListenerResult

  case object Ignored extends ListenerResult

  case class Acknowledged(listenerAcknowledgement: ListenerAcknowledgement) extends ListenerResult

  def simpleStateless(behavior: (TinkerContext[_], NotedTranscription) => ListenerResult)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context

    context.system.gossiper !! Gossiper.SubscribeAccurate(context.messageAdapter(TranscriptionEvent))

    Tinker.receiveMessage {
      case TranscriptionEvent(notedTranscription) =>
        behavior(context, notedTranscription) match {
          case Ignored =>
          case Acknowledged(listenerAcknowledgement) =>
            context.system.chronicler !! listenerAcknowledgement
        }

        Tinker.steadily
    }
  }
}

object RasaAnnotatingListener {
  sealed trait Message
  private case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message

  def apply(subscription: SpiritRef[NotedTranscription] => Gossiper.Subscription, listener: SpiritRef[RasaAnnotatedNotedTranscription])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    context.system.gossiper !! subscription(context.messageAdapter(TranscriptionEvent))

    implicit val scheduler: Scheduler = context.system.actorSystem.scheduler
    implicit val duration: FiniteDuration = 1.seconds // FIXME: hopefully can be faster, or more likely, replaced
    implicit val timeout: Timeout = Timeout(duration)

    Tinker.receiveMessage {
      case TranscriptionEvent(notedTranscription) =>
        val rawText = notedTranscription.capture.whisperResult.whisperResultContent.text

        val maybeRasaResult: Option[RasaResult] = if (rawText.wordCount < 30) {
          Await.ready[RasaResult](context.system.rasaActor.ask(RasaActor.GetRasaResult(rawText, _)), duration).value match {
            case None =>
              context.actorContext.log.warn(s"It looks like the future for ${notedTranscription.noteId} was empty")
              None
            case Some(value) =>
              value match {
                case Failure(exception) =>
                  context.actorContext.log.warn(s"Fetching Rasa result for ${notedTranscription.noteId} failed", exception)
                  None
                case Success(rasaResult) => Some(rasaResult)
              }
          }
        } else {
          None
        }

        listener !! RasaAnnotatedNotedTranscription(notedTranscription, maybeRasaResult)

        Tinker.steadily
    }
  }

  //

  case class RasaAnnotatedNotedTranscription(notedTranscription: NotedTranscription, maybeRasaResult: Option[RasaResult])

  case object MessageListJsonProtocol extends DefaultJsonProtocol {
    import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat
    import me.micseydel.model.RasaResultProtocol.rasaResultFormat
    implicit val RasaAnnotatedNotedTranscriptionJsonFormat: RootJsonFormat[RasaAnnotatedNotedTranscription] =
      jsonFormat2(RasaAnnotatedNotedTranscription)
  }
}
