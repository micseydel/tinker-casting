package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import javax.sound.sampled.{AudioFormat, Clip, DataLine, Mixer, AudioSystem as JVMAudioSystem}
import scala.util.{Failure, Success, Try}

object SoundPlayerActor {
  sealed trait Message

  final case class PlaySound(path: String) extends Message

  final case class ReceiveNotePing(ping: Ping) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("Sound Player", TinkerColor.random(), "🎙️", ReceiveNotePing, Some("_actor_notes")) { (context, noteRef) =>
    context.actorContext.log.info("Refreshing note Markdown")
    noteRef.refreshNote(None)

    Tinker.receiveMessage {
      case PlaySound(path) =>

        noteRef.getPreferredDevice() match {
          case Failure(exception) => throw exception
          case Success(maybeUserPreferredDevice) =>
            context.actorContext.log.info(s"Playing sound at $path (currently ignoring maybeUserPreferredDevice $maybeUserPreferredDevice, just using the default)")

            val audioSystem = new AudioSystem
            audioSystem.playClip(path, maybeUserPreferredDevice) match {
              case Failure(exception) => context.actorContext.log.warn("Failed to play clip", exception)
              case Success(_) =>
            }

            Tinker.steadily
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
    }
  }

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def refreshNote(maybePreferredDeviceName: Option[String]): Try[NoOp.type] = {
      val audioSystem = new AudioSystem

      val (_, fullList) = audioSystem.getMixers

      val markdown = ("Click to refresh" ::
        fullList
          .map(_.getName)
          .filterNot(_.contains("Microphone"))
          .filterNot(_.contains("Port")) // Port mixers work unreliably, in my experience; consider commenting this out though
          .distinct
      ).map("- [ ] " + _).mkString("", "\n", "\n")

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
          )//.getOrElse(targetFormat) // return original if no fallback works
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

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("Sound Player Testing", TinkerColor.random(), "🥼", ReceiveNotePing, Some("_actor_notes")) { (context, noteRef) =>
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
