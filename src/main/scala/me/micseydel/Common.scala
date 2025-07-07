package me.micseydel

import cats.data.ValidatedNel
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.dsl.TinkerClock
import spray.json.{DeserializationException, JsNull, JsNumber, JsString, JsValue, JsonFormat, RootJsonFormat}

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time._
import javax.sound.sampled.AudioSystem
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}
import scala.jdk.StreamConverters._
import scala.util.{Failure, Success}

/**
 * @param filename path (non-full)
 * @param time created or modified depending on what's needed
 */
case class FileMetadata(filename: String, time: ZonedDateTime)

object NoOp

object Common {
  def getStackTraceString(t: Throwable): String = {
    t.toString + "\n" + t.getStackTrace.map("  " + _).mkString("\n")
  }

  private val SpecialCharactersAllowedInActorPaths = "-_.*$+:@&=,!~';.".toSet

  /**
   * Akka actor names are limited, and while a link to the details is hard to find, exceptions read:
   *   Actor paths MUST: not start with `$`, include only ASCII letters and can only contain these special characters: -_.*$+:@&=,!~';.
   *
   * This implies digits are not allowed, but they are - e.g. notename20251020 is known to work.
   *
   * To follow these rules
   * - an underscore prefix is added if it starts with a `$`
   * - spaces are replaced with underscores
   * - disallowed special characters are filtered out
   */
  def tryToCleanForActorName(s: String): String = {
    (if (s.startsWith("$")) {
      "_" + s
    } else {
      s
    }).flatMap {
      case ' ' => Some('_')
      case char if SpecialCharactersAllowedInActorPaths.contains(char) || isAsciiLetterOrDigit(char) => Some(char)
      case _ => None
    }.mkString("")
  }

  private def isAsciiLetterOrDigit(char: Char): Boolean = {
    char.isDigit ||
      (char >= 'a' && char <= 'z') ||
      (char >= 'A' && char <= 'Z')
  }

  /**
   * @return e.g. 2011-12-03T10:15:30
   */
  def getFileCreationTimeISO8601(path: Path): String = {
    getFileCreationTime(path)
      .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  }

  def listDir(path: Path): List[FileMetadata] = {
    Files.list(path)
      .toScala(List)
      .map(path => FileMetadata(path.toString, getFileCreationTime(path)))
  }

  // see: [[Getting file creation time (programming)]]
  def getFileCreationTime(path: Path): ZonedDateTime = {
    val attr = Files.readAttributes(path, classOf[BasicFileAttributes])

    attr.creationTime()
      .toInstant
      .atZone(ZoneId.systemDefault())
  }

  def getFileLastModifiedTime(path: Path): ZonedDateTime = {
    val attr = Files.readAttributes(path, classOf[BasicFileAttributes])

    attr.lastModifiedTime()
      .toInstant
      .atZone(ZoneId.systemDefault())
  }

  implicit object ZonedDateTimeJsonFormat extends RootJsonFormat[ZonedDateTime] {
    def write(t: ZonedDateTime): JsString = JsString(t.toString)

    def read(value: JsValue): ZonedDateTime = value match {
      case JsString(s) =>
        ZonedDateTime.parse(s)
      case JsNumber(value) =>
        try {
          ZonedDateTime.ofInstant(Instant.ofEpochSecond(value.longValue), ZoneId.systemDefault)
        } catch {
          case e: DateTimeException =>
            throw DeserializationException(s"Failed to extract ZonedDateTime from value $value", e)
        }
      case _ =>
        throw DeserializationException("Expected a string or epoch number")
    }
  }

  implicit object PathJsonFormat extends RootJsonFormat[Path] {
    def write(t: Path): JsString = JsString(t.toString)

    def read(value: JsValue): Path = value match {
      case JsString(s) => Path.of(s)
      case _ => throw DeserializationException("Expected a string")
    }
  }

  /**
   * Returns in SECONDS
   */
  def getWavLength(path: String): Double = {
    // source: https://stackoverflow.com/a/3009973/1157440
    val file: File = new File(path)
    val audioInputStream = AudioSystem.getAudioInputStream(file)
    val format = audioInputStream.getFormat
    val frames = audioInputStream.getFrameLength
    val durationInSeconds = (frames + 0.0) / format.getFrameRate
    durationInSeconds
  }

//  /**
//   * Allows for actors to specify a "normal" type in an actor receipt mailbox, and for recipients to define
//   * a temporary one-off mailbox for converting the normal type into whatever type the original actor needs.
//   * (This is otherwise difficult because the typical pattern requires inheritance.)
//   */
//  def messageAdapter[ReceiptType, LocalTargetType](
//                                                    outerContext: ActorContext[LocalTargetType],
//                                                    adapter: ReceiptType => LocalTargetType)(
//                                                    temporaryReceiptMailboxUser: ActorRef[ReceiptType] => Unit
//                                                  ): ActorRef[ReceiptType] = {
//    outerContext.spawnAnonymous(Behaviors.setup[ReceiptType] { temporaryContext: ActorContext[ReceiptType] =>
//      temporaryReceiptMailboxUser(temporaryContext.self)
//
//      Behaviors.receiveMessage[ReceiptType] { reply: ReceiptType =>
//        val targetMailbox: ActorRef[LocalTargetType] = outerContext.self
//        targetMailbox ! adapter(reply)
//        Behaviors.stopped
//      }
//    })
//  }

  private def timeAgo(dateTime: ZonedDateTime)(implicit tinkerClock: TinkerClock): String = {
    val now = tinkerClock.now()
    val duration = Duration.between(dateTime, now)

    val hours = duration.toHours
    val minutes = duration.toMinutes % 60

    s"$hours hours and $minutes minutes ago"
  }

  object OptionalJsonFormat {
    def apply[Item](itemFormat: JsonFormat[Item]): JsonFormat[Option[Item]] = new JsonFormat[Option[Item]] {

      override def write(optionItem: Option[Item]): JsValue = optionItem match {
        case Some(item) => itemFormat.write(item)
        case None => JsNull
      }

      override def read(json: JsValue): Option[Item] = json match {
        case JsNull => None
        case other => Some(itemFormat.read(other))
      }
    }
  }

  def await[FutureResult, FinalResult](fut: Future[FutureResult], onSuccess: FutureResult => FinalResult)(implicit duration: FiniteDuration): FinalResult = {
    Await.ready(fut, duration).value match {
      case Some(Success(futureResult)) =>
        onSuccess(futureResult)

      case Some(Failure(throwable)) =>
        throw throwable
      case None =>
        throw new RuntimeException(s"Expected ask to be non-empty")
    }
  }

  def getValidatedStringFromConfig(map: Map[String, Any], key: String): ValidatedNel[String, String] = {
    map.get(key) match {
      case Some(value: String) => value.validNel
      case other => s"Expected a string for key $key but found: $other".invalidNel
    }
  }
}
