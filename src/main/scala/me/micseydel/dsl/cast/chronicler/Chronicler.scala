package me.micseydel.dsl.cast.chronicler

import akka.actor.InvalidActorNameException
import akka.actor.typed.ActorRef
import me.micseydel.actor.*
import FolderWatcherActor.Ping
import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.actor.transcription.TranscriptionNoteWrapper
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.TypedMqtt.MqttMessage
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler.ChroniclerJsonProtocol
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.{AutomaticallyIntegrated, NoteState, TranscribedMobileNoteEntry}
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, TypedMqtt}
import me.micseydel.model.*
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.vault.*
import me.micseydel.{Common, NoOp}
import spray.json.*
import ChroniclerJsonProtocol.listenerAcknowledgementJsonFormat
import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import scala.annotation.unused
import scala.util.{Failure, Success, Try}

object Chronicler {

  sealed trait Message

  case class TranscriptionStartedEvent(capture: NoticedAudioNote) extends Message

  case class TranscriptionCompletedEvent(event: WhisperResult) extends Message

  case class ReceiveNotedTranscription(notedTranscription: NotedTranscription) extends Message

  case class ListenerAcknowledgement(noteId: NoteId, noteCreationDate: LocalDate, timeOfAck: ZonedDateTime, details: String, setNoteState: Option[NoteState]) extends Message

  //  final case class ReceiveWavFile(filename: String, bytes: Array[Byte]) extends Message

  final case class ReceiveNotePing(ping: Ping) extends Message

  private case class ReceiveMqtt(mqttMessage: TypedMqtt.MqttMessage) extends Message


  def apply(vaultRoot: VaultPath, gossiper: SpiritRef[Gossiper.Message])(implicit Tinker: Tinker): Ability[Message] =
    initializing(vaultRoot, gossiper)

  private val NoteName = "Chronicler"
  private val Topic = s"[[$NoteName]]"

  private def initializing(
                            vaultRoot: VaultPath,
                            gossiper: SpiritRef[Gossiper.Message]
                          )(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](NoteName, rgb(135, 206, 235), "✍️", ReceiveNotePing, Some("_actor_notes")) { case (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context

    context.system.mqtt ! TypedMqtt.Subscribe(Topic, context.messageAdapter(ReceiveMqtt).underlying)

    context.self !! ReceiveNotePing(NoOp) // bootstrap

    Tinker.receiveMessage {
      case ReceiveNotePing(_) =>
        finishInitializing(vaultRoot, gossiper)

      case other =>
        context.actorContext.log.warn(s"Waiting for necessary config, ignoring message $other")
        Tinker.steadily
    }
  }

  private def finishInitializing(
                                  vaultRoot: VaultPath,
                                  gossiper: SpiritRef[Gossiper.Message]
                                )(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    @unused
    val audioNoteCapturer: ActorRef[AudioNoteCapturer.Message] = context.spawn(AudioNoteCapturer(
      vaultRoot, context.self.underlying
    ), "AudioNoteCapturer")

    val moc: ActorRef[ChroniclerMOC.Message] = context.spawn(ChroniclerMOC(), "ChroniclerMOC")

    behavior(Map.empty)(Tinker, gossiper, moc, audioNoteCapturer)
  }

  private def behavior(wavNameToTranscriptionNoteOwner: Map[String, SpiritRef[TranscriptionNoteWrapper.Message]])
                      (implicit Tinker: Tinker,
                       gossiper: SpiritRef[Gossiper.Message],
                       moc: ActorRef[ChroniclerMOC.Message],
                       audioNoteCapturer: ActorRef[AudioNoteCapturer.Message]
                      ): Ability[Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Currently have ${wavNameToTranscriptionNoteOwner.size} elements")
    Tinker.receiveMessage { message =>
      implicit val c: TinkerContext[_] = context
      context.actorContext.log.debug(s"Chronicler received message $message")
      message match {
        case e@TranscriptionStartedEvent(capture@NoticedAudioNote(audioFilePath, captureTime, durationSeconds, transcriptionStartedTime)) =>
          val audioFileName = audioFilePath.getFileName.toString
          if (!audioFileName.startsWith(".")) {
            context.actorContext.log.info(s"$e")
            val updatedNoteNameToTranscriptionNoteOwner: Map[String, SpiritRef[TranscriptionNoteWrapper.Message]] = wavNameToTranscriptionNoteOwner.get(audioFileName) match {
              case Some(wrapper) =>
                // FIXME: kinda hacky, but let's just not send any redundant events to the wrapper for now (though later)
                wavNameToTranscriptionNoteOwner
              case None =>
                val name = s"TranscriptionNoteWrapper_${audioFilePath.getFileName.toString.slice(21, 36)}"
                val behavior = TranscriptionNoteWrapper(capture, context.self)
                context.actorContext.log.debug(
                  s"Creating note wrapper actor with name $name (wavPath $audioFilePath); " +
                    s"already in wavNameToTranscriptionNoteOwner? ${wavNameToTranscriptionNoteOwner.contains(audioFileName)}")
                try {
                  val wrapper = context.cast(behavior, name)
                  wavNameToTranscriptionNoteOwner.updated(audioFileName, wrapper)
                } catch {
                  case _: InvalidActorNameException =>
                    context.actorContext.log.error(s"Failed to create already-existing actor name $name for wav $audioFileName; ignoring, but this should be looked into")
                    wavNameToTranscriptionNoteOwner
                }
            }

            behavior(updatedNoteNameToTranscriptionNoteOwner)
          } else {
            Tinker.steadily
          }


        case TranscriptionCompletedEvent(result) =>
          val wavName = Path.of(result.whisperResultMetadata.vaultPath).getFileName.toString
          wavNameToTranscriptionNoteOwner.get(wavName) match {
            case None =>
              context.actorContext.log.info(s"Tried to update transcription for $wavName but it wasn't in $wavNameToTranscriptionNoteOwner")
              Tinker.steadily
            case Some(ref) =>
              context.actorContext.log.info(s"Notifying wrapper actor for $wavName of transcription completion")
              ref !! TranscriptionNoteWrapper.TranscriptionCompletedEvent(result)
              Tinker.steadily
          }

        case ReceiveNotedTranscription(notedTranscription) =>
          val captureTime = notedTranscription.capture.captureTime

          // FIXME: this is the attachment...
          context.actorContext.log.info(s"Sending note ${notedTranscription.noteId} to MOC")
          // FIXME: this should not be conditional on the large model, and the receiver needs to be updated to handle it
          moc ! ChroniclerMOC.AddNote(TranscribedMobileNoteEntry(captureTime, notedTranscription.noteId, notedTranscription.capture.whisperResult.whisperResultContent.text.wordCount))

          // tell the actor who will tell the listeners
          // (Gossiper has separate Base / Large listeners / keys)
          gossiper !! Gossiper.Receive(notedTranscription)
          // tinkerbrain is just for tracking purposes
          //        tinkerBrain ! TinkerBrain.Transcription(notedTranscription)

          // and done
          Tinker.steadily

        case ListenerAcknowledgement(noteRef, forDay, timeOfAck, details, setNoteState) =>
          moc ! ChroniclerMOC.ListenerAcknowledgement(noteRef, forDay, timeOfAck, details, setNoteState)
          Tinker.steadily

        case ReceiveNotePing(_) =>
          context.actorContext.log.warn("Ignoring note ping")
          Tinker.steadily

        case ReceiveMqtt(MqttMessage(Topic, payload)) =>
          Try(new String(payload).parseJson.convertTo[ListenerAcknowledgement]) match {
            case Failure(exception) => context.actorContext.log.warn(s"mqtt message deserialization failed of size ${payload.length}", exception)
            case Success(listenerAcknowledgement: ListenerAcknowledgement) =>
              context.actorContext.log.info(s"received a ListenerAcknowledgement via mqtt, forwarding to self now...")
              context.self !! listenerAcknowledgement
          }

          Tinker.steadily

        case ReceiveMqtt(MqttMessage(unexpectedTopic, payload)) =>
          context.actorContext.log.warn(s"Received mqtt message on unexpected topic $unexpectedTopic, payload size ${payload.length}")
          Tinker.steadily
      }
    }
  }

  object ChroniclerJsonProtocol extends DefaultJsonProtocol {

    import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.NoteStateJsonFormat
    import me.micseydel.util.JsonUtil.ZonedDateTimeJsonFormat
    import me.micseydel.vault.LinkIdJsonProtocol.noteIdFormat
    import me.micseydel.util.JsonUtil.CommonJsonProtocol.LocalDateTypeJsonFormat

    implicit val listenerAcknowledgementJsonFormat: JsonFormat[ListenerAcknowledgement] = jsonFormat5(ListenerAcknowledgement(_, _, _, _, _))
  }

  object ListenerAcknowledgement {
    def justIntegrated(noteId: NoteId, forDay: LocalDate, details: String)(implicit tinkerContext: TinkerContext[_]): ListenerAcknowledgement = {
      ListenerAcknowledgement(noteId, forDay, tinkerContext.system.clock.now(), details, Some(AutomaticallyIntegrated))
    }
  }
}
