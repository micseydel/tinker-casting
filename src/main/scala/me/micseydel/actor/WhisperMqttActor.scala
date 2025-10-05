package me.micseydel.actor

import akka.actor.typed.ActorRef
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TypedMqtt.MqttMessage
import me.micseydel.dsl.{Tinker, TinkerContext, TypedMqtt}
import me.micseydel.model.WhisperModel
import me.micseydel.vault.VaultKeeper
import spray.json.{DefaultJsonProtocol, JsonFormat, enrichAny}

import java.util.Base64
import scala.util.{Failure, Success, Try}


object WhisperMqttActor {
  sealed trait Message
  case class Enqueue(vaultPath: String) extends Message

  private case class ReceiveAttachment(result: (String, Try[Array[Byte]])) extends Message

  private case class ReceiveMqtt(message: MqttMessage) extends Message

  def apply(replyTo: ActorRef[AudioNoteCapturer.TranscriptionEvent], model: WhisperModel)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val Topic = s"${context.self.path}(model::${model.simpleName})"
    val OutTopic = s"python/transcription/${model.simpleName}"

    context.system.typedMqtt ! TypedMqtt.Subscribe(Topic, context.messageAdapter(ReceiveMqtt).underlying)
    context.actorContext.log.info(s"Subscribed to $Topic")

    Tinker.receiveMessage {
      case Enqueue(vaultPath) =>
        context.system.vaultKeeper !! VaultKeeper.RequestAttachmentContents(vaultPath, context.messageAdapter(ReceiveAttachment).underlying)
        Tinker.steadily

      case ReceiveMqtt(MqttMessage(Topic, payload)) =>
        replyTo ! AudioNoteCapturer.TranscriptionEvent(new String(payload))
        Tinker.steadily

      case ReceiveAttachment((vaultPath, result)) =>
        result match {
          case Failure(exception) => context.actorContext.log.warn(s"Failed to read $vaultPath", exception)
          case Success(attachmentContents) =>
            import WhisperMqttJsonProtocol.outMessageJsonFormat
            val outMessage = OutMessage(Topic, vaultPath, Base64.getEncoder.encodeToString(attachmentContents)).toJson
            val outMessageBytes = outMessage.compactPrint.getBytes
            // per https://stackoverflow.com/a/34525013/1157440
            if (outMessageBytes.length > 268435456) {
              context.actorContext.log.warn(s"Ignoring $vaultPath, too big at ${outMessageBytes.length} bytes")
            } else {
              context.system.typedMqtt ! TypedMqtt.Publish(OutTopic, outMessageBytes)
            }
        }

        Tinker.steadily

      case other =>
        context.actorContext.log.warn(s"did not expect $other (Topic=$Topic)")
        Tinker.steadily
    }
  }

  private object WhisperMqttJsonProtocol extends DefaultJsonProtocol {
    implicit val outMessageJsonFormat: JsonFormat[OutMessage] = jsonFormat3(OutMessage)
  }

  private case class OutMessage(responseTopic: String, vaultPath: String, b64Encoded: String)
}
