package me.micseydel.testsupport

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.cast.TinkerBrain
import me.micseydel.dsl.cast.TinkerBrain.{SentMessage, TranscriptionBroadcast}

//private
object TinkerBrainForTesting {
  def apply(): Behavior[TinkerBrain.Message] = Behaviors.receive { (context, message) =>

    message match {
      //      case message: TinkerBrain.PersistedMessage => ???
      case sentMessage: SentMessage =>
        TestHelpers.log(s"Message sent: $sentMessage")
        context.log.info(s"Message sent: $sentMessage")
      case transcriptionBroadcast: TranscriptionBroadcast =>
        context.log.info(s"Transcription broadcast: $transcriptionBroadcast")
      case _ =>
      //      case TinkerBrain.WriteNote(tinker) => ???
      //      case TinkerBrain.SpiritCast(actorPath) => ???
      //      case TinkerBrain.SystemStarted() => ???
      //      case TinkerBrain.RegisterClient(replyTo) => ???
    }

    context.log.info(s"Received but ignoring $message")
    Behaviors.same
  }
}
