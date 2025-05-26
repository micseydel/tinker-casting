package me.micseydel.dsl.cast.chronicler

import akka.actor.InvalidActorNameException
import akka.actor.typed.ActorRef
import me.micseydel.{Common, NoOp}
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.AudioNoteCapturer.NoticedAudioNote
import me.micseydel.actor._
import me.micseydel.actor.transcription.TranscriptionNoteWrapper
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.{AutomaticallyIntegrated, NoteState, TranscribedMobileNoteEntry}
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.model._
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.vault._
import me.micseydel.vault.persistence.NoteRef

import java.nio.file.{Path, Paths}
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import scala.annotation.unused
import scala.util.{Failure, Success, Try}

object Chronicler {

  // mailbox

  sealed trait Message

  sealed trait PostTinkeringInitMessage extends Message

  case class TranscriptionStartedEvent(capture: NoticedAudioNote) extends PostTinkeringInitMessage

  case class TranscriptionCompletedEvent(event: WhisperResult) extends PostTinkeringInitMessage

  case class ActOnNoteRef(event: NotedTranscription) extends PostTinkeringInitMessage

  case class ListenerAcknowledgement(noteId: NoteId, timeOfAck: ZonedDateTime, details: String, setNoteState: Option[NoteState]) extends PostTinkeringInitMessage

  final case class ReceiveNotePing(ping: Ping) extends PostTinkeringInitMessage


  def apply(config: ChroniclerConfig, gossiper: SpiritRef[Gossiper.Message])(implicit Tinker: Tinker): Ability[Message] =
    initializing(config.vaultRoot, gossiper, config.eventReceiverHost, config.eventReceiverPort)

  case class ChroniclerConfig(vaultRoot: VaultPath, eventReceiverHost: String, eventReceiverPort: Int)

  private def initializing(
                        vaultRoot: VaultPath,
                        gossiper: SpiritRef[Gossiper.Message],
                        whisperEventReceiverHost: String,
                        whisperEventReceiverPort: Int
                      )(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("Chronicler", rgb(135, 206, 235), "✍️", ReceiveNotePing) { case (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context
    context.self !! ReceiveNotePing(NoOp) // bootstrap
    Tinker.receiveMessage {
      case ReceiveNotePing(_) =>
        noteRef.properties match {
          case Success(Some(config)) =>
            finishInitializing(vaultRoot, gossiper, config, whisperEventReceiverHost, whisperEventReceiverPort)

          case Failure(exception) =>
            throw exception

          case Success(None) =>
            context.actorContext.log.warn(s"Note checked but the contents weren't valid, expected: {audioWatchPath, whisperLarge, whisperBase, whisperEventReceiverHost, whisperEventReceiverPort, rasaHost} where the port is an int")
            Tinker.steadily
        }

      case other =>
        context.actorContext.log.warn(s"Waiting for necessary config, ignoring message $other")
        Tinker.steadily
    }
  }

  private def finishInitializing(
                        vaultRoot: VaultPath,
                        gossiper: SpiritRef[Gossiper.Message],
                        config: AudioNoteCaptureProperties,
                        whisperEventReceiverHost: String, whisperEventReceiverPort: Int
                      )(implicit Tinker: Tinker): Ability[Message] =  Tinker.setup { context =>
    @unused
    val audioNoteCapturer: ActorRef[AudioNoteCapturer.Message] = context.spawn(AudioNoteCapturer(
      vaultRoot, context.self.underlying, config, whisperEventReceiverHost, whisperEventReceiverPort
    ), "AudioNoteCapturer")

    val moc: ActorRef[ChroniclerMOC.Message] = context.spawn(ChroniclerMOC(), "ChroniclerMOC")

    behavior(Map.empty)(Tinker, gossiper, moc)
  }

  private def behavior(wavNameToTranscriptionNoteOwner: Map[String, SpiritRef[TranscriptionNoteWrapper.Message]])
                      (implicit Tinker: Tinker,
                       gossiper: SpiritRef[Gossiper.Message],
                       moc: ActorRef[ChroniclerMOC.Message]
                      ): Ability[Message] =  Tinker.setup { context =>
    context.actorContext.log.info(s"Behavior initialized with ${wavNameToTranscriptionNoteOwner.size} elements")
    Tinker.receiveMessage { message =>
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
              val name = s"TranscriptionNoteWrapper_${wavPath.getFileName.toString.slice(21, 36)}"
              val behavior = TranscriptionNoteWrapper(capture, context.self)
              context.actorContext.log.debug(
                s"Creating note wrapper actor with name $name (wavPath $wavPath); " +
                  s"already in wavNameToTranscriptionNoteOwner? ${wavNameToTranscriptionNoteOwner.contains(wavName)}")
              try {
                val wrapper = context.cast(behavior, name)
                wavNameToTranscriptionNoteOwner.updated(wavName, wrapper)
              } catch {
                case _: InvalidActorNameException =>
                  context.actorContext.log.warn(s"Failed to create already-existing actor name $name for wav $wavName")
                  wavNameToTranscriptionNoteOwner
              }
          }

          behavior(updatedNoteNameToTranscriptionNoteOwner)

        case TranscriptionCompletedEvent(result) =>
          val wavName = Path.of(result.whisperResultMetadata.vaultPath).getFileName.toString
          wavNameToTranscriptionNoteOwner.get(wavName) match {
            case None =>
              context.actorContext.log.error(s"Tried to update transcription for $wavName but it wasn't in $wavNameToTranscriptionNoteOwner")
              Tinker.steadily
            case Some(ref) =>
              // FIXME: new ChroniclerMOCActor, has an AmendLine(captureTime) message
              //                ChroniclerMOC.AppendToLine(captureTime, contents)
              context.actorContext.log.info(s"Notifying wrapper actor for $wavName of transcription completion")
              ref !! TranscriptionNoteWrapper.TranscriptionCompletedEvent(result)
              Tinker.steadily
          }

        case ActOnNoteRef(notedTranscription) =>
          val captureTime = notedTranscription.capture.captureTime

          // add to the MOC, but just for the large model
          if (notedTranscription.capture.whisperResult.whisperResultMetadata.model == LargeModel) {
            // FIXME: this is the attachment...
            context.actorContext.log.info(s"Sending note ${notedTranscription.noteId} to MOC")
            moc ! ChroniclerMOC.AddNote(TranscribedMobileNoteEntry(captureTime, notedTranscription.noteId, notedTranscription.capture.whisperResult.whisperResultContent.text.wordCount))
          } else {
            context.actorContext.log.debug(s"Ignoring ${notedTranscription.noteId} because ${notedTranscription.capture.whisperResult.whisperResultMetadata.model} != LargeModel")
          }

          // tell the actor who will tell the listeners
          // (Gossiper has separate Base / Large listeners / keys)
          gossiper !! Gossiper.Receive(notedTranscription)
          // tinkerbrain is just for tracking purposes
          //        tinkerBrain ! TinkerBrain.Transcription(notedTranscription)

          // and done
          Tinker.steadily

        case ListenerAcknowledgement(noteRef, timeOfAck, details, setNoteState) =>
          moc ! ChroniclerMOC.ListenerAcknowledgement(noteRef, timeOfAck, details, setNoteState)
          Tinker.steadily

        case ReceiveNotePing(_) =>
          context.actorContext.log.warn("Ignoring note ping")
          Tinker.steadily
      }
    }
  }

  // util
  private val mp3Pattern = """mobile_audio_capture_(\d{8}-\d{6})\.mp3""".r

  // e.g. desktop_audio_capture_20240417-184238.wav
  private val desktopPattern = """desktop_audio_capture_(\d{8}-\d{6})\.wav""".r

  // e.g. mobile_audio_capture_20240416-204756.wav
  private val mobilePattern = """mobile_audio_capture_(\d{8}-\d{6})\.wav""".r
  private val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

  def tryParse(dateTimeStr: String): Either[String, ZonedDateTime] = {
    Try(LocalDateTime.parse(dateTimeStr, formatter)) match {
      case Success(localDateTime) =>
        // FIXME: get the timezone by location of the device at the time; assumed California for foreseeable future
        Right(ZonedDateTime.of(localDateTime, ZoneId.systemDefault()))
      case Failure(e) =>
        Left(s"Invalid date format:\n${Common.getStackTraceString(e)}")
    }
  }

  // FIXME: should only be used in one place
  def getCaptureTimeFromAndroidAudioPath(wavPath: Path): Either[String, ZonedDateTime] = {
    // e.g. desktop_audio_capture_2024-04-17_13_45_26.wav
    //    val desktopPattern = """desktop_audio_capture_(\d{4}-\d{2}-\d{2}_\d{2}_\d{2}_\d{2})\.wav""".r

    getCaptureTimeFromAndroidAudioPath(wavPath.getFileName.toString)
  }

  def getCaptureTimeFromAndroidAudioPath(filename: String): Either[String, ZonedDateTime] = filename match {
    case mobilePattern(dateTimeStr) => tryParse(dateTimeStr)
    case desktopPattern(dateTimeStr) => tryParse(dateTimeStr)
    case mp3Pattern(dateTimeStr) => tryParse(dateTimeStr)

    case _ => Left(s"Filename does not match the expected formats: $mobilePattern or $desktopPattern")
  }

  object ListenerAcknowledgement {
    def justIntegrated(noteId: NoteId, details: String)(implicit tinkerContext: TinkerContext[_]): ListenerAcknowledgement = {
      ListenerAcknowledgement(noteId, tinkerContext.system.clock.now(), details, Some(AutomaticallyIntegrated))
    }
  }

  // FIXME: untangle chronicler and audionotecapture
  case class AudioNoteCaptureProperties(audioWatchPath: Path, whisperLarge: String, whisperBase: String)

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def properties: Try[Option[AudioNoteCaptureProperties]] = {
      noteRef.readNote().flatMap(_.yamlFrontMatter).map { properties =>
        for {
          audioWatchPath <- properties.get("audioWatchPath").map(_.asInstanceOf[String]).map(Paths.get(_))
          whisperLarge <- properties.get("whisperLarge").map(_.asInstanceOf[String])
          whisperBase <- properties.get("whisperBase").map(_.asInstanceOf[String])
        } yield AudioNoteCaptureProperties(audioWatchPath, whisperLarge, whisperBase)
      }
    }
  }
}
