package me.micseydel.dsl.cast.chronicler

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.Common
import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.actor._
import me.micseydel.actor.transcription.TranscriptionNoteWrapper
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.{AutomaticallyIntegrated, NoteState, TranscribedMobileNoteEntry}
import me.micseydel.dsl.cast.{Gossiper, TinkerBrain}
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext, Tinkerer}
import me.micseydel.model._
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.vault._

import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import scala.annotation.unused
import scala.concurrent.ExecutionContextExecutorService
import scala.util.{Failure, Success, Try}

object Chronicler {

  // mailbox

  sealed trait Message

  case class StartTinkering(tinker: Tinker) extends Message

  sealed trait PostInitMessage extends Message

  case class TranscriptionStartedEvent(capture: NoticedAudioNote) extends PostInitMessage

  case class TranscriptionCompletedEvent(event: WhisperResult) extends PostInitMessage

  case class ActOnNoteRef(noteId: NoteId, event: NotedTranscription) extends PostInitMessage

  case class ListenerAcknowledgement(noteRef: NoteId, timeOfAck: ZonedDateTime, details: String, setNoteState: Option[NoteState]) extends PostInitMessage


  def apply(config: AppConfig, vaultKeeper: ActorRef[VaultKeeper.Message], gossiper: ActorRef[Gossiper.Message], tinkerBrain: ActorRef[TinkerBrain.Message])(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = Behaviors.setup { context =>
    val rasaActor = context.spawn(
      RasaActor(config.whisperLargeHost),
      "RasaActor"
    )

    initializing(config, vaultKeeper, rasaActor, gossiper, tinkerBrain)
  }

  private def initializing(config: AppConfig,
                           vaultKeeper: ActorRef[VaultKeeper.Message],
                           rasaActor: ActorRef[RasaActor.Message],
                           gossiper: ActorRef[Gossiper.Message],
                           tinkerBrain: ActorRef[TinkerBrain.Message])(implicit httpExecutionContext: ExecutionContextExecutorService): Behavior[Message] = Behaviors.withStash(10) { stash =>
    Behaviors.receive[Message] { (context, message) =>
      message match {
        case StartTinkering(tinker) =>
          implicit val Tinker: Tinker = tinker

          // FIXME: move this after StartTinkering
          @unused
          val audioNoteCapturer: ActorRef[AudioNoteCapturer.Message] = context.spawn(AudioNoteCapturer(
            AudioNoteCapturer.Config(
              config.vaultRoot,
              config.audioNoteWatchPath,
              config.whisperBaseHost,
              config.whisperLargeHost,
              config.eventReceiverHost,
              config.eventReceiverPort
            ), context.self, tinkerBrain
          ), "AudioNoteCapturer")

          val jsonPath = config.vaultRoot.resolve("json")
          val moc = context.spawn(ChroniclerMOC(jsonPath, vaultKeeper), "ChroniclerMOC")
          stash.unstashAll(behavior(jsonPath, vaultKeeper, moc, rasaActor, tinker.tinkerSystem.wrap(gossiper), tinkerBrain, Map.empty))

        case message: PostInitMessage =>
          stash.stash(message)
          Behaviors.same
      }
    }
  }

  def behavior(
                jsonPath: Path,
                vaultKeeper: ActorRef[VaultKeeper.Message],
                moc: ActorRef[ChroniclerMOC.Message],
                rasaActor: ActorRef[RasaActor.Message],
                gossiper: SpiritRef[Gossiper.Message],
                tinkerBrain: ActorRef[TinkerBrain.Message],
                wavNameToTranscriptionNoteOwner: Map[String, SpiritRef[TranscriptionNoteWrapper.Message]]
      )(implicit Tinker: Tinker): Ability[Message] = Tinkerer(rgb(135, 206, 235), "✍️").receive { (context, message) =>
        implicit val c: TinkerContext[_] = context
        context.actorContext.log.debug(s"Chronicler received message $message")
        message match {
          case e@TranscriptionStartedEvent(capture@NoticedAudioNote(wavPath, captureTime, durationSeconds, transcriptionStartedTime)) =>
            context.actorContext.log.info(s"$e")

            val wavName = wavPath.getFileName.toString

            val updatedNoteNameToTranscriptionNoteOwner: Map[String, SpiritRef[TranscriptionNoteWrapper.Message]] = wavNameToTranscriptionNoteOwner.get(wavName) match {
              case Some(wrapper) =>
                // FIXME: kinda hacky, but let's just not send any redundant events to the wrapper for now (though later)
                wavNameToTranscriptionNoteOwner
              case None =>
                val wrapper = {
                  val name = s"TranscriptionNoteWrapper_${wavPath.getFileName.toString.slice(21, 36)}"
                  val behavior = TranscriptionNoteWrapper(capture, rasaActor, context.self)
                  context.actorContext.log.debug(s"Creating note wrapper actor with name $name (wavPath $wavPath)")
                  context.cast(behavior, name)
                }

                wavNameToTranscriptionNoteOwner.updated(wavName, wrapper)
            }

            behavior(jsonPath, vaultKeeper, moc, rasaActor, gossiper, tinkerBrain, updatedNoteNameToTranscriptionNoteOwner)

          case TranscriptionCompletedEvent(result) =>
            val wavName = Path.of(result.whisperResultMetadata.vaultPath).getFileName.toString
            wavNameToTranscriptionNoteOwner.get(wavName) match {
              case None =>
                context.actorContext.log.error(s"Tried to update transcription for $wavName but it wasn't in $wavNameToTranscriptionNoteOwner")
                Behaviors.same
              case Some(ref) =>
                // FIXME: new ChroniclerMOCActor, has an AmendLine(captureTime) message
                //                ChroniclerMOC.AppendToLine(captureTime, contents)
                context.actorContext.log.info(s"Notifying wrapper actor for $wavName of transcription completion")
                ref !! TranscriptionNoteWrapper.TranscriptionCompletedEvent(result)
                Behaviors.same
            }

          case ActOnNoteRef(noteId, notedTranscription) =>
            val captureTime = notedTranscription.capture.captureTime

            // add to the MOC, but just for the large model
            if (notedTranscription.capture.whisperResult.whisperResultMetadata.model == LargeModel) {
              // FIXME: this is the attachment...
              context.actorContext.log.info(s"Sending note ${notedTranscription.noteId} to MOC")
              moc ! ChroniclerMOC.AddNote(TranscribedMobileNoteEntry(captureTime, noteId, notedTranscription.capture.whisperResult.whisperResultContent.text.wordCount))
            } else {
              context.actorContext.log.debug(s"Ignoring ${notedTranscription.noteId} because ${notedTranscription.capture.whisperResult.whisperResultMetadata.model} != LargeModel")
            }

            // tell the actor who will tell the listeners
            // (Gossiper has separate Base / Large listeners / keys)
            gossiper !! Gossiper.Receive(notedTranscription)
            // tinkerbrain is just for tracking purposes
            //        tinkerBrain ! TinkerBrain.Transcription(notedTranscription)

            // quick bit of logging...
            notedTranscription.rasaResult match {
              case Some(KnownIntent.no_intent(maybeCat)) =>
                context.actorContext.log.debug(s"Intent no_intent had entit(ies): $maybeCat")
              case _ =>
            }

            // and done
            Behaviors.same

          case ListenerAcknowledgement(noteRef, timeOfAck, details, setNoteState) =>
            moc ! ChroniclerMOC.ListenerAcknowledgement(noteRef, timeOfAck, details, setNoteState)
            Behaviors.same

          case StartTinkering(system) =>
            context.actorContext.log.warn(s"Received an unexpected (extra) tinker system: $system")
            Behaviors.same
        }
      }

  // util

  // FIXME: should only be used in one place
  def getCaptureTimeFromAndroidAudioPath(wavPath: Path): Either[String, ZonedDateTime] = {
    // e.g. desktop_audio_capture_2024-04-17_13_45_26.wav
//    val desktopPattern = """desktop_audio_capture_(\d{4}-\d{2}-\d{2}_\d{2}_\d{2}_\d{2})\.wav""".r

    val mp3Pattern = """mobile_audio_capture_(\d{8}-\d{6})\.mp3""".r

    // e.g. desktop_audio_capture_20240417-184238.wav
    val desktopPattern = """desktop_audio_capture_(\d{8}-\d{6})\.wav""".r

    // e.g. mobile_audio_capture_20240416-204756.wav
    val mobilePattern = """mobile_audio_capture_(\d{8}-\d{6})\.wav""".r
    val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

    def tryParse(dateTimeStr: String): Either[String, ZonedDateTime] = {
      Try(LocalDateTime.parse(dateTimeStr, formatter)) match {
        case Success(localDateTime) =>
          // FIXME: get the timezone by location of the device at the time; assumed California for foreseeable future
          Right(ZonedDateTime.of(localDateTime, ZoneId.systemDefault()))
        case Failure(e) =>
          Left(s"Invalid date format:\n${Common.getStackTraceString(e)}")
      }
    }

    wavPath.getFileName.toString match {
      case mobilePattern(dateTimeStr) => tryParse(dateTimeStr)
      case desktopPattern(dateTimeStr) => tryParse(dateTimeStr)
      case mp3Pattern(dateTimeStr) => tryParse(dateTimeStr)

      case _ => Left(s"Filename does not match the expected formats: $mobilePattern or $desktopPattern")
    }
  }

  object ListenerAcknowledgement {
    def justIntegrated(noteId: NoteId, details: String)(implicit tinkerContext: TinkerContext[_]): ListenerAcknowledgement = {
      ListenerAcknowledgement(noteId, tinkerContext.system.clock.now(), details, Some(AutomaticallyIntegrated))
    }
  }
}
