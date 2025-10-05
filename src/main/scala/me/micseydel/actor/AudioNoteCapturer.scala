package me.micseydel.actor

import akka.actor.typed.ActorRef
import com.softwaremill.quicklens.*
import me.micseydel.Common
import me.micseydel.actor.AudioNoteCapturerHelpers.*
import me.micseydel.actor.FolderWatcherActor.{PathCreatedEvent, PathModifiedEvent, Ping}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TypedMqtt.MqttMessage
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TypedMqtt}
import me.micseydel.model.WhisperResultJsonProtocol.*
import me.micseydel.model.{BaseModel, LargeModel, TurboModel, WhisperResult}
import me.micseydel.vault.VaultPath
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.vault.persistence.NoteRef.FileDoesNotExist
import spray.json.*

import java.io.File
import java.nio.file.{Path, Paths}
import java.time.ZonedDateTime
import scala.annotation.unused
import scala.concurrent.ExecutionContextExecutorService
import scala.util.{Failure, Success, Try}

object AudioNoteCapturer {
  sealed trait Message

  case class TranscriptionEvent(payload: String) extends Message

  private case class MqttTranscriptionEvent(mqttMessage: MqttMessage) extends Message

  private case class AudioPathUpdatedEvent(event: FolderWatcherActor.PathUpdatedEvent) extends Message

  private case class ReceivePing(ping: Ping) extends Message

  //  final case class ReceiveWavFile(filename: String, bytes: Array[Byte]) extends Message

  // behavior

  private val NoteName = "Audio Note Capture"

  def apply(vaultRoot: VaultPath, chronicler: ActorRef[Chronicler.Message])(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing](NoteName, TinkerColor.random(), "🎤", ReceivePing, Some("_actor_notes")) { case (context, noteRef) =>
    noteRef.properties match {
      case Failure(exception) => throw exception
      case Success(None) =>
        context.actorContext.log.warn(s"No note found for ${noteRef.noteId}, going to sleep since no config is available...")
        Tinker.ignore

      case Success(Some(properties)) =>
        context.actorContext.log.info(s"Using properties $properties")
        finishInitializing(vaultRoot, properties, chronicler)(Tinker, noteRef, context.system.httpExecutionContext)
    }
  }

  private def finishInitializing(vaultRoot: VaultPath, config: AudioNoteCaptureProperties, chronicler: ActorRef[Chronicler.Message])(implicit Tinker: Tinker, noteRef: NoteRef, ec: ExecutionContextExecutorService): Ability[Message] = Tinker.setup { context =>
    val newFileCreationEventAdapter = context.messageAdapter(AudioPathUpdatedEvent).underlying
    @unused // receives messages from a thread it creates, we don't send it messages but the adapter lets it reply to us
    val folderWatcherActor = context.spawn(
      FolderWatcherActor(
        config.audioWatchPath, newFileCreationEventAdapter
      ),
      "MobileAudioFolderWatcherActor"
    )

    // FIXME: these need to keep track of queued message, track/log things that don't come back in time
    val recipients: List[SpiritRef[WhisperMqttActor.Message]] = List(
      context.cast(WhisperMqttActor(context.self.underlying, BaseModel), "WhisperMqttActor_base"),
      context.cast(WhisperMqttActor(context.self.underlying, TurboModel), "WhisperMqttActor_turbo"),
      context.cast(WhisperMqttActor(context.self.underlying, LargeModel), "WhisperMqttActor_large")
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
          context.actorContext.log.debug("Sending TranscriptionStartedEvent to {}", recipients)

          val transcriptionStartTime = clock.now()
          val capture = NoticedAudioNote(audioPath, captureTime, Common.getWavLength(audioPath.toString), transcriptionStartTime)
          chronicler ! Chronicler.TranscriptionStartedEvent(capture)

          val vaultPath = audioPath.toString.replace(vaultRoot.toString + "/", "")
          val enqueueRequest = WhisperMqttActor.Enqueue(vaultPath)
          recipients.foreach(_ !!!! enqueueRequest)
      }
    }

    behavior(
      vaultRoot,
      chronicler,
      triggerTranscriptionForAudioPath,
      config.audioWatchPath
    )(Tinker, noteRef)
  }

  private def behavior(vaultRoot: VaultPath, chronicler: ActorRef[Chronicler.Message], triggerTranscriptionForWavPath: Path => Unit, audioWatchPath: Path)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case AudioPathUpdatedEvent(PathCreatedEvent(audioPath)) if validPath(audioPath) =>
        context.actorContext.log.info(s"New audio ${audioPath.getFileName} (size=${new File(audioPath.toString).length()})")

        Try(Common.getWavLength(audioPath.toString)) match {
          case Failure(exception) =>
            context.actorContext.log.warn(s"Failed to get .wav length for $audioPath", exception)
          case Success(seconds) =>
            context.actorContext.log.debug(s"wavPath $audioPath is $seconds seconds long, triggering transcription")
        }

        triggerTranscriptionForWavPath(audioPath)

        val transcriptionNoteName = AudioNoteCapturerHelpers.transcriptionNoteName(audioPath)
        noteRef.addToList(s"[[$transcriptionNoteName]]") match {
          case Failure(exception) => context.actorContext.log.error(s"Failed to add $transcriptionNoteName", exception)
          case Success(result) =>
            if (result) {
              context.actorContext.log.info(s"Added $transcriptionNoteName to the list")
            } else {
              context.actorContext.log.warn(s"Failed to add $transcriptionNoteName to the list")
            }
        }

        Tinker.steadily

      case AudioPathUpdatedEvent(PathModifiedEvent(modifiedPath)) if validPath(modifiedPath) =>
        context.actorContext.log.warn(s"Detected but ignoring modified path: $modifiedPath")
        Tinker.steadily

      case AudioPathUpdatedEvent(PathModifiedEvent(modifiedPath)) =>
        context.actorContext.log.debug(s"MODIFICATION ${modifiedPath.getFileName} (was this Syncthing doing a partial sync? size=${new File(modifiedPath.toString).length()})")
        Tinker.steadily
      case AudioPathUpdatedEvent(other) =>
        context.actorContext.log.debug(s"Ignoring event $other")
        Tinker.steadily

      case TranscriptionEvent(payload) =>
        Try(payload.parseJson.convertTo[WhisperResult]) match {
          case Failure(exception) => context.actorContext.log.error(s"Failed to deserialize transcription; payload=$payload", exception)
          case Success(whisperResultEvent) =>
            if (!pathSupportedByFfmpeg(whisperResultEvent.whisperResultMetadata.vaultPath.toLowerCase)) {
              context.actorContext.log.warn(s"Whisper result filename ${whisperResultEvent.whisperResultMetadata.vaultPath} expected to end with $FFMPEGFormats!")
            }

            if (whisperResultEvent.whisperResultMetadata.model == LargeModel) {
              val transcriptionNoteName = AudioNoteCapturerHelpers.transcriptionNoteName(vaultRoot.resolve(whisperResultEvent.whisperResultMetadata.vaultPath))
              context.actorContext.log.info(s"Removing from list: $transcriptionNoteName")
              noteRef.removeFromList(s"[[$transcriptionNoteName]]") match {
                case Failure(exception) => context.actorContext.log.error(s"Failed to remove $transcriptionNoteName", exception)
                case Success(removed) =>
                  if (removed) {
                    context.actorContext.log.info(s"Removed $transcriptionNoteName")
                  } else {
                    context.actorContext.log.warn(s"Failed to remove $transcriptionNoteName")
                  }
              }
            }

            context.actorContext.log.info(s"Transcription completed for ${whisperResultEvent.whisperResultMetadata.vaultPath}")
            chronicler ! Chronicler.TranscriptionCompletedEvent(AudioNoteCapturerHelpers.fixWhisper(whisperResultEvent))
        }

        Tinker.steadily

      case MqttTranscriptionEvent(MqttMessage(topic, payload)) =>
        context.self !!!! TranscriptionEvent(new String(payload))

        Tinker.steadily

      case ReceivePing(_) =>
        context.actorContext.log.debug("Ignoring ping, already initialized")
        Tinker.steadily

      //      case m@ReceiveWavFile(filename, bytes) =>
      ////        writeWavWithJavax(audioWatchPath.resolve(filename), bytes)
      //        // FIXME
      //        context.actorContext.log.warn(s"Ignoring $m, never finished impl")
      //        Tinker.steadily
    }
  }


  // model

  case class NoticedAudioNote(wavPath: Path, captureTime: ZonedDateTime, lengthSeconds: Double, transcriptionStartedTime: ZonedDateTime)
}

object AudioNoteCapturerHelpers {
  case class AudioNoteCaptureProperties(audioWatchPath: Path, whisperLarge: Option[String], whisperBase: Option[String], whisperTurbo: Option[String])

  def fixWhisper(whisperResultEvent: WhisperResult): WhisperResult = {
    whisperResultEvent
      .modify(_.whisperResultContent.text)
      .using(_.replace("f***ing", "fucking"))
  }

  private case class Entry(line: String)

  implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def properties: Try[Option[AudioNoteCaptureProperties]] = {
      noteRef.readNote().flatMap(_.yamlFrontMatter).map { properties =>
        for {
          audioWatchPath <- properties.get("audioWatchPath").map(_.asInstanceOf[String]).map(Paths.get(_))
          whisperLarge = properties.get("whisperLarge").map(_.asInstanceOf[String])
          whisperBase = properties.get("whisperBase").map(_.asInstanceOf[String])
          whisperTurbo = properties.get("whisperTurbo").map(_.asInstanceOf[String])
        } yield AudioNoteCaptureProperties(audioWatchPath, whisperLarge, whisperBase, whisperTurbo)
      }
    }

    private def getList(): Try[Either[NoteRef.FileDoesNotExist.type, List[Entry]]] = {
      noteRef.readMarkdownSafer() match {
        case NoteRef.Contents(diskResult) =>
          diskResult.flatMap { markdown =>
            val lines = markdown.split("\n").toList
            Try(lines.map(Entry)).map(Right.apply)
          }
        case NoteRef.FileDoesNotExist => Success(Left(NoteRef.FileDoesNotExist))
      }
    }

    def addToList(toAdd: String): Try[Boolean] = {
      val entry = Entry(s"- $toAdd")
      getList().flatMap {
        case Left(FileDoesNotExist) =>
          noteRef.setMarkdown(entry.line).map(_ => false)
        case Right(entries) =>
          val alreadyPresent = entries.contains(entry)
          if (alreadyPresent) {
            Success(false)
          } else {
            noteRef
              .setMarkdown((entry :: entries).map(_.line).mkString("\n"))
              .map(_ => true)
          }
      }
    }

    def removeFromList(toRemove: String): Try[Boolean] = {
      val entry = Entry(s"- $toRemove")
      getList().flatMap {
        case Left(FileDoesNotExist) =>
          noteRef.setMarkdown(entry.line).map(_ => false)
        case Right(entries) =>
          val present = entries.contains(entry)
          if (present) {
            noteRef
              .setMarkdown(entries.map(_.line).filterNot(_ == entry.line).mkString("\n"))
              .map(_ => true)
          } else {
            Success(false)
          }
      }
    }
  }

  // FIXME: dynamically pull this from `ffmpeg -formats`
  val FFMPEGFormats: Set[String] = Set("3dostr", "3g2", "3gp", "4xm", "a64", "aa", "aac", "aax", "ac3", "ac4", "ace", "acm", "act", "adf", "adp", "ads", "adts", "adx", "aea", "afc", "aiff", "aix", "alaw", "alias_pix", "alp", "amr", "amrnb", "amrwb", "amv", "anm", "apac", "apc", "ape", "apm", "apng", "aptx", "aptx_hd", "aqtitle", "argo_asf", "argo_brp", "argo_cvg", "asf", "asf_o", "asf_stream", "ass", "ast", "au", "audiotoolbox", "av1", "d", "avi", "avif", "avm2", "avr", "avs", "avs2", "avs3", "bethsoftvid", "bfi", "bfstm", "bin", "bink", "binka", "bit", "bitpacked", "bmp_pipe", "bmv", "boa", "bonk", "brender_pix", "brstm", "c93", "caf", "cavsvideo", "cdg", "cdxl", "cine", "codec2", "codec2raw", "concat", "crc", "cri_pipe", "dash", "data", "daud", "dcstr", "dds_pipe", "derf", "dfa", "dfpwm", "dhav", "dirac", "dnxhd", "dpx_pipe", "dsf", "dsicin", "dss", "dts", "dtshd", "dv", "dvbsub", "dvbtxt", "dvd", "dxa", "ea", "ea_cdata", "eac3", "epaf", "evc", "exr_pipe", "f32be", "f32le", "f4v", "f64be", "f64le", "ffmetadata", "fifo", "film_cpk", "filmstrip", "fits", "flac", "flic", "flv", "framecrc", "framehash", "framemd5", "frm", "fsb", "fwse", "g722", "g723_1", "g726", "g726le", "g729", "gdv", "gem_pipe", "genh", "gif", "gif_pipe", "gsm", "gxf", "h261", "h263", "h264", "hash", "hca", "hcom", "hdr_pipe", "hds", "hevc", "hls", "hnm", "iamf", "ico", "idcin", "idf", "iff", "ifv", "ilbc", "image2", "image2pipe", "imf", "ingenient", "ipmovie", "ipod", "ipu", "ircam", "ismv", "iss", "iv8", "ivf", "ivr", "j2k_pipe", "jacosub", "jpeg_pipe", "jpegls_pipe", "jpegxl_anim", "jpegxl_pipe", "jv", "kux", "kvag", "laf", "latm", "d", "live_flv", "lmlm4", "loas", "lrc", "luodat", "lvf", "lxf", "m4v", "matroska", "matroska,webm", "mca", "mcc", "md5", "mgsts", "microdvd", "mjpeg", "mjpeg_2000", "mkvtimestamp_v2", "mlp", "mlv", "mm", "mmf", "mods", "moflex", "mov", "mov,mp4,m4a,3gp,3g2,mj2", "mp2", "mp3", "mp4", "mpc", "mpc8", "mpeg", "mpeg1video", "mpeg2video", "mpegts", "mpegtsraw", "mpegvideo", "mpjpeg", "mpl2", "mpsub", "msf", "msnwctcp", "msp", "mtaf", "mtv", "mulaw", "musx", "mv", "mvi", "mxf", "mxf_d10", "mxf_opatom", "mxg", "nc", "nistsphere", "nsp", "nsv", "null", "nut", "nuv", "obu", "oga", "ogg", "ogv", "oma", "opus", "osq", "paf", "pam_pipe", "pbm_pipe", "pcx_pipe", "pdv", "pfm_pipe", "pgm_pipe", "pgmyuv_pipe", "pgx_pipe", "phm_pipe", "photocd_pipe", "pictor_pipe", "pjs", "pmp", "png_pipe", "pp_bnk", "ppm_pipe", "psd_pipe", "psp", "psxstr", "pva", "pvf", "qcp", "qdraw_pipe", "qoa", "qoi_pipe", "r3d", "rawvideo", "rcwt", "realtext", "redspark", "rka", "rl2", "rm", "roq", "rpl", "rsd", "rso", "rtp", "rtp_mpegts", "rtsp", "s16be", "s16le", "s24be", "s24le", "s32be", "s32le", "s337m", "s8", "sami", "sap", "sbc", "sbg", "scc", "scd", "sdl,sdl2", "sdns", "sdp", "sdr2", "sds", "sdx", "segment", "ser", "sga", "sgi_pipe", "shn", "siff", "simbiosis_imx", "sln", "smjpeg", "smk", "smoothstreaming", "smush", "sol", "sox", "spdif", "spx", "srt", "stl", "stream_segment,ssegment", "streamhash", "subviewer", "subviewer1", "sunrast_pipe", "sup", "svag", "svcd", "svg_pipe", "svs", "swf", "tak", "tedcaptions", "tee", "thp", "tiertexseq", "tiff_pipe", "tmv", "truehd", "tta", "ttml", "tty", "txd", "ty", "u16be", "u16le", "u24be", "u24le", "u32be", "u32le", "u8", "uncodedframecrc", "usm", "v210", "v210x", "vag", "vbn_pipe", "vc1", "vc1test", "vcd", "vidc", "vividas", "vivo", "vmd", "vob", "vobsub", "voc", "vpk", "vplayer", "vqf", "vvc", "w64", "wady", "wav", "wavarc", "wc3movie", "webm", "webm_chunk", "webm_dash_manifest", "webp", "webp_pipe", "webvtt", "wsaud", "wsd", "wsvqa", "wtv", "wv", "wve", "d", "xa", "xbin", "xbm_pipe", "xmd", "xmv", "xpm_pipe", "xvag", "xwd_pipe", "xwma", "yop", "yuv4mpegpipe")

  def supportedByFfmpeg(audioPath: Path): Boolean = pathSupportedByFfmpeg(audioPath.toString)

  def pathSupportedByFfmpeg(audioPath: String): Boolean = audioPath.split("\\.").lastOption.exists(FFMPEGFormats.contains)

  def validPath(audioPath: Path): Boolean = {
    !audioPath.startsWith(".evr_recently_deleted") && supportedByFfmpeg(audioPath)
  }

  def transcriptionNoteName(wavPath: Path): String = s"Transcription for ${wavPath.getFileName.toString}"
}
