package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import com.softwaremill.quicklens._
import me.micseydel.Common
import me.micseydel.actor.FolderWatcherActor.{PathCreatedEvent, PathModifiedEvent}
import me.micseydel.dsl.{Tinker, TinkerClock}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.{TinkerBrain, UntrackedTimeKeeper}
import me.micseydel.model.WhisperResult
import me.micseydel.util.TimeUtil
import me.micseydel.vault.VaultPath
import me.micseydel.vault.persistence.NoteRef

import java.io.File
import java.nio.file.Path
import java.time.ZonedDateTime
import java.util.UUID
import javax.sound.sampled.UnsupportedAudioFileException
import scala.annotation.unused
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object AudioNoteCapturer {
  case class Config(
                     vaultRoot: VaultPath,
                     audioNoteWatchPath: Path,
                     whisperBaseHost: String,
                     whisperLargeHost: String,
                     eventReceiverHost: String,
                     eventReceiverPort: Int
                   )

  val AcceptableFileExts: Set[String] = Set(
    "wav", "mp3", "flac", "m4a",
    "mp4", "mkv", "wmv", "flv"
  )

  // mailbox

  sealed trait Message

  case class TranscriptionEvent(event: WhisperResult) extends Message

  private case class PathUpdatedEvent(event: FolderWatcherActor.PathUpdatedEvent) extends Message

  // behavior

  private val NoteName = "audio_note_capture (tinkering)"
  private val NoteFolder = "_actor_notes/audio_note_capture"

  def apply(config: Config, chronicler: ActorRef[Chronicler.Message], tinkerbrain: ActorRef[TinkerBrain.Message])(
    implicit Tinker: Tinker, httpExecutionContext: ExecutionContext
  ): Ability[Message] = Tinker.initializedWithNote(NoteName, NoteFolder) { case (context, noteRef) =>
    val newFileCreationEventAdapter = context.messageAdapter(PathUpdatedEvent).underlying
    @unused // receives messages from a thread it creates, we don't send it messages but the adapter lets it reply to us
    val folderWatcherActor = context.spawn(
      FolderWatcherActor(
        config.audioNoteWatchPath, newFileCreationEventAdapter
      ),
      "MobileAudioFolderWatcherActor"
    )

    @unused // driven internally by an HTTP server (which takes in response to the async WhisperFlask uses
    val eventReceiver: ActorRef[EventReceiver.Message] = context.spawn(
      EventReceiver(
        EventReceiver.Config(config.eventReceiverHost, config.eventReceiverPort),
        context.messageAdapter(TranscriptionEvent).underlying,
        tinkerbrain
      ),
      "EventReceiver"
    )

    // FIXME: bad names, secondary means transfer the file over the network
    // maybe on_host, on_local_network?
    val primaryWhisperFlaskAmbassador: ActorRef[WhisperFlaskAmbassador.Message] = context.spawn(
      SecondaryWhisperFlaskAmbassador(SecondaryWhisperFlaskAmbassador.Config(
        config.whisperLargeHost,
        config.eventReceiverHost,
        config.eventReceiverPort,
        config.vaultRoot
      )),
      "PrimaryWhisperFlaskAmbassador"
    )

    val secondaryWhisperFlaskAmbassador = context.spawn(
      SecondaryWhisperFlaskAmbassador(SecondaryWhisperFlaskAmbassador.Config(
        config.whisperBaseHost,
        config.eventReceiverHost,
        config.eventReceiverPort,
        config.vaultRoot
      )),
      "SecondaryWhisperFlaskAmbassador"
    )

    val clock = context.system.clock
    def triggerTranscriptionForAudioPath(audioPath: Path): Unit = {
      Chronicler.getCaptureTimeFromAndroidAudioPath(audioPath) match {
        case Left(msg) =>
          if (audioPath.toString.toLowerCase.endsWith(".tmp")) {
            context.actorContext.log.debug(s"Because of .tmp, ignoring would-be message:::Failed to get capture time for wavPath $audioPath: $msg")
          } else {
            context.actorContext.log.error(s"Failed to get capture time for wavPath $audioPath: $msg")
          }
        case Right(captureTime) =>
          context.actorContext.log.debug(s"Sending TranscriptionStartedEvent to wrapper")

          val transcriptionStartTime = clock.now()
          val capture = NoticedAudioNote(audioPath, captureTime, Common.getWavLength(audioPath.toString), transcriptionStartTime)
          chronicler ! Chronicler.TranscriptionStartedEvent(capture)

          val enqueueRequest = WhisperFlaskAmbassador.Enqueue(audioPath.toString.replace(config.vaultRoot.toString + "/", ""))
          primaryWhisperFlaskAmbassador ! enqueueRequest
          secondaryWhisperFlaskAmbassador ! enqueueRequest
      }
    }

    val timerKey: Option[UUID] = Some(UUID.randomUUID())

    // FIXME: change this interface if possible, then swap out for a regular context.castTimeKeeper or whatever
    val timeKeeper: ActorRef[UntrackedTimeKeeper.Message] = context.spawn(UntrackedTimeKeeper(), "UntrackedTimeKeeper")

    idle(
      config.vaultRoot,
      chronicler,
      timerKey,
      timeKeeper,
      triggerTranscriptionForAudioPath
    )(Tinker, noteRef)
  }

  private def idle(vaultRoot: VaultPath, chronicler: ActorRef[Chronicler.Message], timerKey: Option[UUID], timeKeeper: ActorRef[UntrackedTimeKeeper.Message], triggerTranscriptionForWavPath: Path => Unit)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case PathUpdatedEvent(PathCreatedEvent(audioPath)) if audioPath.toString.split("\\.").lastOption.exists(AcceptableFileExts.contains) =>
        context.actorContext.log.info(s"New audio ${audioPath.getFileName} (size=${new File(audioPath.toString).length()})")
        val seconds: Double = Common.getWavLength(audioPath.toString)
        if (seconds > 58 && seconds < 61) {
          context.actorContext.log.debug(s"wavPath $audioPath is $seconds seconds long, waiting to see if it's partial")
          noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] because seconds (58 < $seconds < 61), entering preventingRedundancy\n")
          preventingRedundancy(vaultRoot, chronicler, timerKey, timeKeeper, triggerTranscriptionForWavPath)
        } else {
          context.actorContext.log.debug(s"wavPath $audioPath is $seconds seconds long, triggering transcription")
          triggerTranscriptionForWavPath(audioPath)
          noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] last triggered transcription for created path ${audioPath.getFileName.toString}\n")
          Tinker.steadily
        }

      case PathUpdatedEvent(PathModifiedEvent(modifiedPath)) if modifiedPath.toString.split("\\.").lastOption.exists(AcceptableFileExts.contains) =>
        context.actorContext.log.warn(s"(need to fix this inefficiency) Re-transcribing $modifiedPath")

        triggerTranscriptionForWavPath(modifiedPath)

        noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] last ==RE==triggered transcription for ==MODIFIED== path ${modifiedPath.getFileName.toString}\n")

        Tinker.steadily

      case PathUpdatedEvent(PathModifiedEvent(modifiedPath)) =>
        context.actorContext.log.debug(s"MODIFICATION ${modifiedPath.getFileName} (was this Syncthing doing a partial sync? size=${new File(modifiedPath.toString).length()})")
        Tinker.steadily
      case PathUpdatedEvent(other) =>
        context.actorContext.log.debug(s"Ignoring event $other")
        Tinker.steadily

      case TranscriptionEvent(whisperResultEvent) =>
        context.actorContext.log.info(s"[idle] Transcription completed for ${whisperResultEvent.whisperResultMetadata.vaultPath}")
        chronicler ! Chronicler.TranscriptionCompletedEvent(fixWhisper(whisperResultEvent))
        noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] last received $whisperResultEvent\n")
        Tinker.steadily
    }
  }

  private def preventingRedundancy(vaultRoot: VaultPath, chronicler: ActorRef[Chronicler.Message], timerKey: Option[UUID], timeKeeper: ActorRef[UntrackedTimeKeeper.Message], triggerTranscriptionForWavPath: Path => Unit)(implicit Tinker: Tinker, noteRef: NoteRef): Behavior[Message] = Tinker.receive { (context, message) =>
    message match {
      case TranscriptionEvent(whisperResultEvent) =>
        context.actorContext.log.info(s"[preventingRedundancy] Transcription completed for ${whisperResultEvent.whisperResultMetadata.vaultPath}")
        chronicler ! Chronicler.TranscriptionCompletedEvent(fixWhisper(whisperResultEvent))
        noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] [preventingRedundancy] $whisperResultEvent]")
        Tinker.steadily

      case PathUpdatedEvent(PathCreatedEvent(path)) =>
        if (!path.endsWith(".tmp")) {
          context.actorContext.log.info(s"Detected updated path $path")
          triggerTranscriptionForWavPath(path)
          timeKeeper ! UntrackedTimeKeeper.Cancel(timerKey)
          noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] [preventingRedundancy] detected a tmp path update, switching to idle")
          idle(vaultRoot, chronicler, timerKey, timeKeeper, triggerTranscriptionForWavPath)
        } else {
          context.actorContext.log.warn(s"Ignoring path update $path")
          Tinker.steadily
        }

      case PathUpdatedEvent(PathModifiedEvent(wavPath)) =>
        Try(Common.getWavLength(wavPath.toString)) match {
          case Failure(exception: UnsupportedAudioFileException) =>
            context.actorContext.log.error(s"Failed to get wav length for $wavPath", exception)
            noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] Failed to get wav length for $wavPath")
            Tinker.steadily
          case Failure(exception) =>
            context.actorContext.log.error(s"Unknown error for $wavPath", exception)
            Tinker.steadily
          case Success(durationSeconds) =>
            //        val durationSeconds = Common.getWavLength(wavPath.toString)
            // if this was completed within 2 seconds of a full minute...
            if (Math.abs(durationSeconds / 60 % 60 - 50) < 2) {
              context.actorContext.log.info(s"Setting a ~1-minute (75s) timer because durationSeconds looked suspicious...")
              timeKeeper ! UntrackedTimeKeeper.RemindMeIn(75.seconds, context.self.underlying, PathUpdatedEvent(PathCreatedEvent(wavPath)), timerKey)
              noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] setting a timer for ${wavPath.toString} and switching to idle")
              Tinker.steadily
            } else {
              context.actorContext.log.info(s"$wavPath modification treating as created (durationSeconds=$durationSeconds)")
              triggerTranscriptionForWavPath(wavPath)
              timeKeeper ! UntrackedTimeKeeper.Cancel(timerKey)
              noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] Triggered for ${wavPath.toString}, switching to idle (no redundancyPrevention)")
              idle(vaultRoot, chronicler, timerKey, timeKeeper, triggerTranscriptionForWavPath)
            }
        }

      case PathUpdatedEvent(FolderWatcherActor.PathDeletedEvent(path)) =>
        context.actorContext.log.debug(s"Ignoring path deletion for $path")
        Tinker.steadily
    }
  }

  //

  case class NoticedAudioNote(wavPath: Path, captureTime: ZonedDateTime, lengthSeconds: Double, transcriptionStartedTime: ZonedDateTime) {
    def transcriptionNoteName: String = s"Transcription for ${wavPath.getFileName.toString}"
  }

  private def fixWhisper(whisperResultEvent: WhisperResult): WhisperResult = {
    whisperResultEvent
      .modify(_.whisperResultContent.text)
      .using(_.replace("f***ing", "fucking"))
  }

  private def now(clock: TinkerClock): String = TimeUtil.zonedDateTimeToISO8601(clock.now())
}
