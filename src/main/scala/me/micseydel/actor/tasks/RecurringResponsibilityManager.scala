package me.micseydel.actor.tasks

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, Tinker, TinkerColor}
import me.micseydel.vault.persistence.NoteRef
import org.dmfs.rfc5545.DateTime
import org.yaml.snakeyaml.{DumperOptions, Yaml}

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import scala.jdk.CollectionConverters.{MapHasAsJava, SeqHasAsJava, SetHasAsJava}
import org.dmfs.rfc5545.recur.{InvalidRecurrenceRuleException, RecurrenceRule}

import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object RecurringResponsibilityManager {
  sealed trait Message

  final case class Track(wikilink: String, localDate: LocalDate) extends Message

  def apply()(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = NoteMakingTinkerer("Recurring responsibilities (MOC)", TinkerColor.random(), "⛑️") { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.readListOfWikiLinks() match {
      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"Something went wrong setting up ${noteRef.noteId}: $e")
        Tinker.ignore
      case Validated.Valid(recurringResponsibilities: NonEmptyList[Entry]) =>
        val specificResponsibilityTrackers = recurringResponsibilities.map { recurringResponsibility =>
          context.actorContext.log.info(s"Casting $recurringResponsibility")
          context.cast(RecurringResponsibilityActor(recurringResponsibility.wikilink, context.self), Common.tryToCleanForActorName(recurringResponsibility.wikilink))
        }

        behavior(recurringResponsibilities)
    }
  }

  private def behavior(tracking: NonEmptyList[Entry])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case Track(wikilink, localDate) =>
        val now = context.system.clock.now()
        val filtered: List[Entry] = tracking.filter(_.wikilink != wikilink)
        val updatedState: NonEmptyList[Entry] = NonEmptyList(Entry(wikilink, Option(localDate)), filtered)
          .sortBy(_.maybeNextActionDate.map(_.toEpochSecond(now.toLocalTime, now.getOffset)).getOrElse(0L))

        noteRef.setMarkdown(updatedState.map {
          case Entry(wikilink, Some(nextActionDate)) =>
            s"[[$wikilink]] ($nextActionDate)"
          case Entry(wikilink, None) =>
            s"[[$wikilink]]"
        }.toList.mkString("- ", "\n- ", "\n"))

        behavior(updatedState)
    }
  }

  //

  private case class Entry(wikilink: String, maybeNextActionDate: Option[LocalDate])

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def readListOfWikiLinks(): ValidatedNel[String, NonEmptyList[Entry]] = {
      noteRef.readMarkdown().map(readListOfWikiLinks) match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(result) =>
          result
      }
    }

    private def readListOfWikiLinks(markdown: String): ValidatedNel[String, NonEmptyList[Entry]] = {
      val lines = markdown.split("\n")

      val (problems: List[String], parsed: List[Entry]) = lines.toList.zipWithIndex.partitionMap {
        case (line, i) =>
          if (line.startsWith("- [[") && line.endsWith("]]")) {
            Right(Entry(line.drop(4).dropRight(2), None))
          } else if (line.startsWith("- [[") && line.contains("]]")) {
            line.split("]] ").toList match {
              case List(wikiLinkPlusPrefixCruft, almostLocalDate) =>
                Try(LocalDate.parse(almostLocalDate.drop(1).dropRight(1))) match {
                  case Failure(exception) =>
                    Left(Common.getStackTraceString(exception))
                  case Success(localDate) =>
                    Right(Entry(wikiLinkPlusPrefixCruft.drop(4), Some(localDate)))
                }

              case other =>
                Left(s"Unrecognized format on line $i: $other")
            }
          } else {
            Left(s"Unrecognized format on line $i: $line")
          }
      }

      problems match {
        case head :: tail =>
          NonEmptyList(head, tail).invalid
        case Nil =>
          parsed match {
            case head :: tail =>
              NonEmptyList(head, tail).valid
            case Nil =>
              "No lines found".invalidNel
          }
      }
    }
  }
}

//https://github.com/jcazevedo/moultingyaml
object TaskNote {
  //  sealed trait TaskNoteStatus
  //  case object Open extends TaskNoteStatus
  //  case object InProgress extends TaskNoteStatus
  //  case object Complete extends TaskNoteStatus
  //
  //  sealed trait TaskNotePriority
  //  case object Low extends TaskNotePriority
  //  case object Normal extends TaskNotePriority
  //  case object High extends TaskNotePriority

  case class TimeEntry(startTime: ZonedDateTime, endTime: ZonedDateTime)

  private val ModelYaml =
    """
      |title: "Complete documentation"
      |status: "in-progress"
      |due: "2024-01-20"
      |priority: "high"
      |contexts: ["work"]
      |projects: ["[[Website Redesign]]"]
      |timeEstimate: 120
      |timeEntries:
      |  - startTime: "2024-01-15T10:30:00Z"
      |    endTime: "2024-01-15T11:15:00Z"
      |recurrence: "FREQ=WEEKLY;BYDAY=MO"
      |complete_instances: ["2024-01-08"]
      |""".stripMargin

  // RecurrenceRule doesn't respect `==` but its string SEEMS consistent...
  case class RecurrenceWrapper(rr: RecurrenceRule) {
    override def equals(obj: Any): Boolean = obj match {
      case other: RecurrenceWrapper =>
        rr.toString == other.rr.toString
      case _ => false
    }

    override def hashCode(): Int = rr.toString.hashCode()

    override def toString: String = rr.toString
  }

  case class ModelTaskNote(
                            title: String,
                            status: String, //TaskNoteStatus,
                            due: LocalDate, // DateTime,
                            priority: String, //TaskNotePriority,
                            contexts: List[String],
                            projects: List[String], //FIXME? NoteId
                            timeEstimate: Int, //minutes
                            timeEntries: List[TimeEntry],
                            recurrence: RecurrenceWrapper,
                            complete_instances: List[LocalDate] // FIXME or ZonedDateTime?
                          ) {
    def recurrenceRule: RecurrenceRule = recurrence.rr
  }

  import net.jcazevedo.moultingyaml.*

  object TestProtocol extends DefaultYamlProtocol {
    implicit object ZonedDateTimeYamlFormat extends YamlFormat[ZonedDateTime] {
      def write(c: ZonedDateTime): YamlValue = YamlString(c.toString)

      def read(value: YamlValue): ZonedDateTime = {
        value match {
          case YamlString(maybeCorrectFormat) =>
            val using = if (maybeCorrectFormat.endsWith("Z")) maybeCorrectFormat.dropRight(1) else maybeCorrectFormat
            // FIXME: HACKY
            ZonedDateTime.of(LocalDateTime.parse(using, DateTimeFormatter.ISO_LOCAL_DATE_TIME), ZoneOffset.UTC)
          case other => deserializationError(s"Expected a DateTime string but found $other")
        }
      }
    }

    implicit object LocalDateYamlFormat extends YamlFormat[LocalDate] {
      def write(c: LocalDate): YamlValue = YamlString(c.toString)

      def read(value: YamlValue): LocalDate = {
        value match {
          case YamlString(maybeDateTime) =>
            LocalDate.parse(maybeDateTime)
          case other => deserializationError(s"Expected a DateTime string but found $other")
        }
      }
    }

    implicit object RecurrenceRuleYamlFormat extends YamlFormat[RecurrenceWrapper] {
      def write(c: RecurrenceWrapper): YamlValue = YamlString(c.toString)

      def read(value: YamlValue): RecurrenceWrapper = {
        value match {
          case YamlString(maybeDateTime) =>
            Try(new RecurrenceRule(maybeDateTime)) match {
              case Failure(exception: InvalidRecurrenceRuleException) => deserializationError(s"It seems that something went wrong parsing the `recurrence` RecurrenceRule: $maybeDateTime", exception)
              case Failure(exception) => deserializationError("Something wonky happened", exception)
              case Success(rrule) => RecurrenceWrapper(rrule)
            }
          case other => deserializationError(s"Expected a RecurrenceRule STRING but found $other")
        }
      }
    }

    implicit val timeEntryYamlFormat: YamlFormat[TimeEntry] = yamlFormat2(TimeEntry)
    implicit val modelTaskNoteYamlFormat: YamlFormat[ModelTaskNote] = yamlFormat10(ModelTaskNote)
  }

  private implicit class RichSnakey(val obj: YamlValue) extends AnyVal {
    def snakeYamlObject: Object = obj match {
      // ugh I wish there was an easy way to impose an order on the keys' output
      case YamlObject(fields) =>
        fields.map {
          case (k, v) =>
            k.snakeYamlObject -> v.snakeYamlObject
        }.asJava
      case YamlArray(elements) => elements.map(_.snakeYamlObject).asJava
      case YamlSet(set) => set.map(_.snakeYamlObject).asJava
      case YamlString(value) => value
      case YamlNumber(value) => value match {
        case v if v.ulp.isWhole => value.underlying.toBigInteger
        case _ => value
      }
      case YamlNaN => Double.NaN.asInstanceOf[java.lang.Double]
      case YamlPositiveInf => Double.PositiveInfinity.asInstanceOf[java.lang.Double]
      case YamlNegativeInf => Double.NegativeInfinity.asInstanceOf[java.lang.Double]
      case YamlDate(date) => date.toDate
      case YamlBoolean(boolean) => if (boolean) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE
      case YamlNull => null
    }
  }

  class SnakeYamlPrinterTEST(
                              flowStyle: FlowStyle = FlowStyle.DEFAULT,
                              scalarStyle: ScalarStyle = ScalarStyle.DEFAULT,
                              lineBreak: LineBreak = LineBreak.DEFAULT)
    extends YamlPrinter(flowStyle, scalarStyle, lineBreak) {

    def dumperOptions: DumperOptions = {
      val dp = new DumperOptions
      dp.setDefaultScalarStyle(scalarStyle.toDumperOption)
      dp.setDefaultFlowStyle(flowStyle.toDumperOption)
      dp.setLineBreak(lineBreak.toDumperOption)
      dp
    }

    override def apply(value: YamlValue): String = {
      new Yaml(dumperOptions).dump(value.snakeYamlObject)
    }
  }


  import TestProtocol.modelTaskNoteYamlFormat

  def main(args: Array[String]): Unit = {
    val yamlValue: YamlValue = ModelYaml.parseYaml

    val modelTaskNote = yamlValue.convertTo[ModelTaskNote]
    println(modelTaskNote)

    println("---")
    print(yamlValue.print(new SnakeYamlPrinterTEST()))
    println("---")

    println(modelTaskNote.recurrence)
    val recurrenceRule: RecurrenceRule = modelTaskNote.recurrenceRule
    val it = recurrenceRule.iterator(DateTime.today())
    val nextDateTime: DateTime = it.nextDateTime() // ew, side effect; stick in a LazyList?
    println(nextDateTime)
    println(LocalDate.parse(nextDateTime.toString, DateTimeFormatter.ofPattern("yyyyMMdd")))

    val inAndOut = modelTaskNote.toYaml.print.parseYaml.convertTo[ModelTaskNote]
    println(modelTaskNote.recurrence == inAndOut.recurrence)
    println(modelTaskNote.copy(recurrence = null) == inAndOut.copy(recurrence = null)) // HA... but also damn
    println(modelTaskNote)
    println(inAndOut)

    //    while (it.hasNext() && rule.isInfinite())
    //    {
    //      val nextInstance = it.nextDateTime();
    //      // do something with nextInstance
    //    }
    // adapted from https://stackoverflow.com/questions/43319008/how-to-parse-an-ical-rrule-in-java
  }
}
