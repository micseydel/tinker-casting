package me.micseydel.dsl.tinkerer

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler.ListenerAcknowledgement
import me.micseydel.dsl.{Tinker, TinkerContext}
import me.micseydel.model.NotedTranscription

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
