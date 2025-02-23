package me.micseydel.actor

import akka.actor.typed.{ActorRef, Behavior}
import com.softwaremill.quicklens._
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.{Common, NoOp}
import me.micseydel.actor.EventReceiver.TranscriptionCompleted
import me.micseydel.actor.FolderWatcherActor.{PathCreatedEvent, PathModifiedEvent}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.UntrackedTimeKeeper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.dsl.{Tinker, TinkerClock, TinkerColor, TinkerContext}
import me.micseydel.model.WhisperResult
import me.micseydel.model.WhisperResultJsonProtocol._
import me.micseydel.util.TimeUtil
import me.micseydel.vault.VaultPath
import me.micseydel.vault.persistence.NoteRef
import spray.json._

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.time.ZonedDateTime
import java.util.UUID
import javax.sound.sampled.UnsupportedAudioFileException
import scala.annotation.unused
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
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

  case class TranscriptionEvent(payload: String) extends Message

  private case class AudioPathUpdatedEvent(event: FolderWatcherActor.PathUpdatedEvent) extends Message
  private case class ReceivePing(ping: Ping) extends Message

  // behavior

  private val NoteName = "Audio Note Capture"

  // FIXME: Chronicler is passed here because it takes a broader message
  def apply(config: Config, chronicler: ActorRef[Chronicler.Message])(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing](NoteName, TinkerColor.random(), "🎤", ReceivePing) { case (context, noteRef) =>
    initializing(config, chronicler)(Tinker, noteRef, context.system.httpExecutionContext)
  }

  def initializing(config: Config, chronicler: ActorRef[Chronicler.Message])(implicit Tinker: Tinker, noteRef: NoteRef, ec: ExecutionContextExecutorService): Ability[Message] = Tinker.setup { context =>
    noteRef.properties match {
      case Success(Some((audioWatchPath, whisperLarge, whisperBase, whisperEventReceiverHost, whisperEventReceiverPort))) =>
        context.actorContext.log.info("Finishing initializing")
        finishInitializing(config.vaultRoot, audioWatchPath, whisperLarge, whisperBase, whisperEventReceiverHost, whisperEventReceiverPort, chronicler)

      case nonSuccess =>
        nonSuccess match {
          case Failure(exception) =>
            context.actorContext.log.warn(s"Something went wrong reading the properties from disk; will wait for disk update", exception)

          case Success(None) =>
            context.actorContext.log.warn("Read from disk successfully but no frontmatter/properties; will wait for disk update")

          case Success(Some(_)) => ??? // FIXME: for the compiler
        }

        Tinker.receiveMessage {
          case ReceivePing(_) =>
            context.actorContext.log.warn("Received ping, trying to read from disk again")
            initializing(config, chronicler)

          case e =>
            context.actorContext.log.warn(s"Ignoring event $e")
            Tinker.steadily
        }
    }
  }

  private def finishInitializing(vaultRoot: VaultPath, audioWatchPath: Path, whisperLarge: String, whisperBase: String, whisperEventReceiverHost: String, whisperEventReceiverPort: Int, chronicler: ActorRef[Chronicler.Message])(implicit Tinker: Tinker, noteRef: NoteRef, ec: ExecutionContextExecutorService): Ability[Message] = Tinker.setup { context =>
    val newFileCreationEventAdapter = context.messageAdapter(AudioPathUpdatedEvent).underlying
    @unused // receives messages from a thread it creates, we don't send it messages but the adapter lets it reply to us
    val folderWatcherActor = context.spawn(
      FolderWatcherActor(
        audioWatchPath, newFileCreationEventAdapter
      ),
      "MobileAudioFolderWatcherActor"
    )

    context.actorContext.log.info(s"Claiming EventReceiver key $TranscriptionCompleted")
    val transcriptionAdapter = context.messageAdapter(TranscriptionEvent).underlying
    context.system.eventReceiver ! EventReceiver.ClaimEventType(TranscriptionCompleted, transcriptionAdapter)

    // FIXME: bad names, secondary means transfer the file over the network
    // maybe on_host, on_local_network?
    val primaryWhisperFlaskAmbassador: ActorRef[WhisperFlaskAmbassador.Message] = context.spawn(
      SecondaryWhisperFlaskAmbassador(SecondaryWhisperFlaskAmbassador.Config(
        whisperLarge,
        whisperEventReceiverHost,
        whisperEventReceiverPort,
        vaultRoot
      )),
      "PrimaryWhisperFlaskAmbassador"
    )

    val secondaryWhisperFlaskAmbassador = context.spawn(
      SecondaryWhisperFlaskAmbassador(SecondaryWhisperFlaskAmbassador.Config(
        whisperBase,
        whisperEventReceiverHost,
        whisperEventReceiverPort,
        vaultRoot
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

          val enqueueRequest = WhisperFlaskAmbassador.Enqueue(audioPath.toString.replace(vaultRoot.toString + "/", ""))
          primaryWhisperFlaskAmbassador ! enqueueRequest
          secondaryWhisperFlaskAmbassador ! enqueueRequest
      }
    }

    val timerKey: Option[UUID] = Some(UUID.randomUUID())

    // FIXME: change this interface if possible, then swap out for a regular context.castTimeKeeper or whatever
    val timeKeeper: ActorRef[UntrackedTimeKeeper.Message] = context.spawn(UntrackedTimeKeeper(), "UntrackedTimeKeeper")

    idle(
      vaultRoot,
      chronicler,
      timerKey,
      timeKeeper,
      triggerTranscriptionForAudioPath
    )(Tinker, noteRef)
  }

  private def idle(vaultRoot: VaultPath, chronicler: ActorRef[Chronicler.Message], timerKey: Option[UUID], timeKeeper: ActorRef[UntrackedTimeKeeper.Message], triggerTranscriptionForWavPath: Path => Unit)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case AudioPathUpdatedEvent(PathCreatedEvent(audioPath)) if audioPath.toString.split("\\.").lastOption.exists(AcceptableFileExts.contains) =>
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

      case AudioPathUpdatedEvent(PathModifiedEvent(modifiedPath)) if modifiedPath.toString.split("\\.").lastOption.exists(AcceptableFileExts.contains) =>
        context.actorContext.log.warn(s"(need to fix this inefficiency) Re-transcribing $modifiedPath")

        triggerTranscriptionForWavPath(modifiedPath)

        noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] last ==RE==triggered transcription for ==MODIFIED== path ${modifiedPath.getFileName.toString}\n")

        Tinker.steadily

      case AudioPathUpdatedEvent(PathModifiedEvent(modifiedPath)) =>
        context.actorContext.log.debug(s"MODIFICATION ${modifiedPath.getFileName} (was this Syncthing doing a partial sync? size=${new File(modifiedPath.toString).length()})")
        Tinker.steadily
      case AudioPathUpdatedEvent(other) =>
        context.actorContext.log.debug(s"Ignoring event $other")
        Tinker.steadily

      case TranscriptionEvent(payload) =>
        onTranscriptionEvent("idle", payload, chronicler, noteRef) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }
        Tinker.steadily

      case ReceivePing(_) =>
        context.actorContext.log.debug("Ignoring ping, already initialized")
        Tinker.steadily
    }
  }

  private def preventingRedundancy(vaultRoot: VaultPath, chronicler: ActorRef[Chronicler.Message], timerKey: Option[UUID], timeKeeper: ActorRef[UntrackedTimeKeeper.Message], triggerTranscriptionForWavPath: Path => Unit)(implicit Tinker: Tinker, noteRef: NoteRef): Behavior[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case TranscriptionEvent(payload) =>
        onTranscriptionEvent("preventingRedundancy", payload, chronicler, noteRef) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }
        Tinker.steadily

      case AudioPathUpdatedEvent(PathCreatedEvent(path)) =>
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

      case AudioPathUpdatedEvent(PathModifiedEvent(wavPath)) =>
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
              timeKeeper ! UntrackedTimeKeeper.RemindMeIn(75.seconds, context.self.underlying, AudioPathUpdatedEvent(PathCreatedEvent(wavPath)), timerKey)
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

      case AudioPathUpdatedEvent(FolderWatcherActor.PathDeletedEvent(path)) =>
        context.actorContext.log.debug(s"Ignoring path deletion for $path")
        Tinker.steadily

      case ReceivePing(_) =>
        context.actorContext.log.debug("Ignoring ping, already initialized")
        Tinker.steadily
    }
  }

  //

  def onTranscriptionEvent(state: String, payload: String, chronicler: ActorRef[Chronicler.Message], noteRef: NoteRef)(implicit context: TinkerContext[_]): Try[NoOp.type] = {
    val whisperResultEvent = try {
      payload.parseJson.convertTo[WhisperResult]

    } catch {
      case e: DeserializationException =>
        context.actorContext.log.error(s"Deserialization failed for payload $payload", e)
        throw e
    }

    if (!whisperResultEvent.whisperResultMetadata.vaultPath.toLowerCase.split("\\.").exists(AcceptableFileExts.contains)) {
      context.actorContext.log.warn(s"Whisper result filename ${whisperResultEvent.whisperResultMetadata.vaultPath} expected to end with $AcceptableFileExts!")
    }

    context.actorContext.log.info(s"[$state] Transcription completed for ${whisperResultEvent.whisperResultMetadata.vaultPath}")
    chronicler ! Chronicler.TranscriptionCompletedEvent(fixWhisper(whisperResultEvent))
    noteRef.setMarkdown(s"- \\[${now(context.system.clock)}] last received $whisperResultEvent\n")
  }

  case class NoticedAudioNote(wavPath: Path, captureTime: ZonedDateTime, lengthSeconds: Double, transcriptionStartedTime: ZonedDateTime) {
    def transcriptionNoteName: String = s"Transcription for ${wavPath.getFileName.toString}"
  }

  private def fixWhisper(whisperResultEvent: WhisperResult): WhisperResult = {
    whisperResultEvent
      .modify(_.whisperResultContent.text)
      .using(_.replace("f***ing", "fucking"))
  }

  private def now(clock: TinkerClock): String = TimeUtil.zonedDateTimeToISO8601(clock.now())

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def properties: Try[Option[(Path, String, String, String, Int)]] = {
      noteRef.readNote().flatMap(_.yamlFrontMatter).map { properties =>
          (for {
            audioWatchPath <- properties.get("audioWatchPath").map(_.asInstanceOf[String]).map(Paths.get(_))
            whisperLarge <- properties.get("whisperLarge").map(_.asInstanceOf[String])
            whisperBase <- properties.get("whisperBase").map(_.asInstanceOf[String])
            whisperEventReceiverHost <- properties.get("whisperEventReceiverHost").map(_.asInstanceOf[String])
            whisperEventReceiverPort <- properties.get("whisperEventReceiverPort").map(_.asInstanceOf[Int])
          } yield (audioWatchPath, whisperLarge, whisperBase, whisperEventReceiverHost, whisperEventReceiverPort))
      }
    }
  }
}
