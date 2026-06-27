package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TypedMqtt.MqttMessage
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext, TypedMqtt}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.io.File
import java.nio.file.Path
import java.time.ZonedDateTime
import javax.sound.sampled.{AudioFormat, Clip, DataLine, LineEvent, Mixer, AudioSystem as JVMAudioSystem}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future, Promise}
import scala.util.{Failure, Success, Try}

object SoundPlayerActor {
  sealed trait Message

  final case class PlaySound(path: String) extends Message

  final case class ReceiveNotePing(ping: Ping) extends Message

  private case class ReceiveMqtt(mqttMessage: MqttMessage) extends Message

  private case object PlayerFinished extends Message

  //

  private type Stopper = () => Unit

  //

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("Sound Player", TinkerColor.random(), "🎙️", ReceiveNotePing, Some("_actor_notes")) { (context, noteRef) =>
    context.actorContext.log.info("Refreshing note Markdown")

    // FIXME: topic should be scoped to the vault, not MERELY use the note id
    val topic = noteRef.noteId.toString
    context.actorContext.log.info(s"Subscribing to mqtt topic $topic")
    context.system.mqtt ! TypedMqtt.Subscribe(topic, context.messageAdapter(ReceiveMqtt).underlying)

    implicit val nr: NoteRef = noteRef

    waiting()
  }

  private def waiting()(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
//    implicit val l: Logger = context.actorContext.log
    noteRef.refreshNote(None)
    Tinker.receiveMessage {
      case PlaySound(path) =>
        noteRef.getPreferredDevice() match {
          case Failure(exception) => throw exception
          case Success(maybeUserPreferredDevice) =>
            context.actorContext.log.info(s"Playing sound at $path (currently ignoring maybeUserPreferredDevice $maybeUserPreferredDevice, just using the default)")

            // FIXME: any reason to keep the old stuff? (maybe merge?)
//            val audioSystem = new AudioSystem
//            audioSystem.playClip(path, maybeUserPreferredDevice) match {
//              case Failure(exception) => context.actorContext.log.warn("Failed to play clip", exception)
//              case Success(_) => context.actorContext.log.info(s"started clip $path")
//            }

//            implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext // FIXME: can I figure out where this artifact came from?
            val wavFile = new File(path)
            val (fut: Future[Unit], stopper: Stopper, lengthMicroseconds: Long) = JvmAudioPlayer.playAsync(wavFile)
            context.pipeToSelf(fut)(_ => PlayerFinished)
            noteRef.refreshNote(
              None, // FIXME - should cache the thing
              Some((wavFile.getName, lengthMicroseconds)))

            playing(stopper)
        }

      case ReceiveNotePing(_) =>
        noteRef.checkForNoteUpdates().flatMap {
          case Some("Click to refresh") =>
            context.actorContext.log.info("Refreshing sound devices")
            noteRef.refreshNote(None)

          case Some(other) =>
            context.actorContext.log.info(s"Setting preferred sound device to $other")
            noteRef.refreshNote(Some(other))

          case None =>
            Success(NoOp)
        } match {
          case Failure(exception) => context.actorContext.log.warn(s"Something went wrong", exception)
          case Success(_) =>
        }

        Tinker.steadily

      case ReceiveMqtt(MqttMessage(topic, payload)) =>
        val path = Path.of(new String(payload)) // FIXME: should use json or something instead
        context.actorContext.log.info(s"Playing sound for $path")
        context.self !! PlaySound(path.toString)
        Tinker.steadily

      case PlayerFinished =>
        Tinker.steadily
    }
  }

  private def playing(stopper: Stopper)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case ReceiveNotePing(_: Ping) =>
        if (noteRef.checkBoxIsChecked()) {
          // FIXME: provide ways over mqtt to - stop, detect a scheduled end time, to receive a finished event
          stopper()
          waiting()
        } else {
          Tinker.steadily
        }

      case PlayerFinished =>
        waiting()

      case other =>
        context.actorContext.log.warn(s"ignoring $other")
        Tinker.steadily
    }
  }

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def refreshNote(maybePreferredDeviceName: Option[String], playing: Option[(String, Long)] = None): Try[NoOp.type] = {
      val audioSystem = new AudioSystem

      val (_, fullList) = audioSystem.getMixers

      val markdown = playing match {
        case None => ("Click to refresh" ::
          fullList
            .map(_.getName)
            .filterNot(_.contains("Microphone"))
            .filterNot(_.contains("Port")) // Port mixers work unreliably, in my experience; consider commenting this out though
            .distinct
          ).map("- [ ] " + _).mkString("", "\n", "\n")
        case Some((filename, microseconds)) =>
          val now = TimeUtil.WithinDayDateTimeFormatter.format(ZonedDateTime.now())
          s"- [ ] Stop /\\ playing: [[$filename]] for ${microseconds}µs (started at $now)\n"
      }

      maybePreferredDeviceName match {
        case Some(newPreferredDeviceName) =>
          noteRef.setTo(Note(markdown, Map("preferredDevice" -> newPreferredDeviceName))).map(_ => NoOp)
        case None =>
          noteRef.setMarkdown(markdown)
      }
    }

    def getPreferredDevice(): Try[Option[String]] = {
      noteRef.readNote().flatMap(_.yamlFrontMatter).map(_.get("preferredDevice").map(_.asInstanceOf[String]))
    }

    def checkForNoteUpdates(): Try[Option[String]] = {
      noteRef.readMarkdown().map { markdown =>
        getFirstChecked(markdown)
      }
    }

    private def getFirstChecked(markdown: String): Option[String] = {
      markdown.split("\n")
        .find(_.startsWith("- [x] "))
        .map(_.drop("- [x] ".length))
    }

    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown().map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }

  private class AudioSystem {
    private val mixerInfo = JVMAudioSystem.getMixerInfo.toList

    /**
     * @return the presumed default if the result is none empty, and the result
     */
    def getMixers: (Option[Mixer.Info], List[Mixer.Info]) = (getPresumedDefault, mixerInfo)

    def playClip(fileSystemPath: String, maybeTarget: Option[String]): Try[NoOp.type] = {
      def getCompatibleFormat(targetFormat: AudioFormat, mixer: Mixer): Option[AudioFormat] = {
        val clipInfo = new DataLine.Info(classOf[Clip], targetFormat)

        if (mixer.isLineSupported(clipInfo)) {
          Some(targetFormat)
        } else {
          // Try common fallback formats
          val fallbackFormats = Array(
            new AudioFormat(44100, 16, 2, true, false), // stereo
            new AudioFormat(44100, 16, 1, true, false), // mono
            new AudioFormat(22050, 16, 2, true, false) // lower sample rate
          )

          fallbackFormats.find(format =>
            mixer.isLineSupported(new DataLine.Info(classOf[Clip], format))
          ) //.getOrElse(targetFormat) // return original if no fallback works
        }
      }

      Try {
        val audioInputStream = JVMAudioSystem.getAudioInputStream(new java.io.File(fileSystemPath))

        val clip: Clip = maybeTarget.flatMap { targetMixer =>
          mixerInfo.filter(_.getName == targetMixer).flatMap { m =>
            val mixer: Mixer = JVMAudioSystem.getMixer(m)
            getCompatibleFormat(audioInputStream.getFormat, mixer).map { format =>
              val clipInfo = new DataLine.Info(classOf[Clip], format)
              mixer.getLine(clipInfo).asInstanceOf[Clip]
            }
          }.headOption
        }.getOrElse(JVMAudioSystem.getClip)

        clip.open(audioInputStream)
        //    val floatGainControl = clip.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
        //    floatGainControl.setValue(gainControl)  //reduce volume by x decibels (like -10f or -20f)
        clip.start() // doesn't block! FIXME: check its interesting docs

        NoOp
      }
    }

    private def getPresumedDefault: Option[Mixer.Info] = mixerInfo match {
      case Nil =>
        None
      case List(justOne) =>
        Some(justOne)
      case multiple =>
        multiple
          .find(_.getName == "Default Audio Device")
          .orElse(multiple.find(_.getName.toLowerCase.startsWith("default")))
          .orElse(multiple.find(_.getName.toLowerCase.contains("default")))
          .orElse(multiple.headOption)
    }
  }
}

object SoundPlayerTestActor {
  sealed trait Message

  final case class ReceiveNotePing(ping: Ping) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("Sound Player Testing", TinkerColor.random(), "🥼", ReceiveNotePing,
    Some("_actor_notes")  // FIXME: remove
  ) { (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context

    val soundPlayerActor = context.cast(SoundPlayerActor(), "SoundPlayerActor")

    noteRef.setMarkdown("- [ ] Play\n")

    Tinker.receiveMessage {
      case ReceiveNotePing(_) =>
        if (noteRef.checkBoxIsChecked()) {
          context.actorContext.log.info(s"Playing sound and refreshing the markdown")
          noteRef.setMarkdown("- [ ] Play\n")
          soundPlayerActor !! SoundPlayerActor.PlaySound(Path)
        }
        Tinker.steadily
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown().map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }

  private val Path = "" // FIXME
}


object JvmAudioPlayer {
  def playAsync(file: File): (scala.concurrent.Future[Unit], () => Unit, Long) = {
    val clip = JVMAudioSystem.getClip()
    clip.open(JVMAudioSystem.getAudioInputStream(file))
    val lengthMicroseconds = clip.getMicrosecondLength

    val donePromise = Promise[Unit]()

    // Listener fires on both natural completion AND manual stop()
    clip.addLineListener(e =>
      if (e.getType == LineEvent.Type.STOP && !donePromise.isCompleted) {
        Try(clip.close())
        donePromise.success(())
      }
    )

    // Start playback in a background thread
    new Thread(() => Try(clip.start())).start()

    val stop: () => Unit = () => {
      if (!donePromise.isCompleted) {
        // FIXME: how hacky is this?
        Try(clip.stop())
        Try(clip.close())
        Try(donePromise.success(()))
      }
    }

    (donePromise.future, stop, lengthMicroseconds)
  }
}
