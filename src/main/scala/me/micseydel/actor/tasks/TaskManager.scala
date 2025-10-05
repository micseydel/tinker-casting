package me.micseydel.actor.tasks

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.tasks.TaskNote.ModelTaskNote
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import net.jcazevedo.moultingyaml.*
import org.dmfs.rfc5545.DateTime
import org.dmfs.rfc5545.recur.{InvalidRecurrenceRuleException, RecurrenceRule}
import org.yaml.snakeyaml.{DumperOptions, Yaml}

import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.*
import scala.annotation.unused
import scala.jdk.CollectionConverters.{MapHasAsJava, SeqHasAsJava, SetHasAsJava}
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.{Failure, Success, Try}

object TaskManager {
  sealed trait Message

//  final case class ReceiveVaultPathUpdatedEvent(event: ?) extends Message

  def apply(taskNotesTasksPath: Path)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
//    @unused // driven by an internal thread
//    val actorNotesFolderWatcher = context.spawn(
//      FolderWatcherActor(
//        taskNotesTasksPath,
//        context.cast(VaultPathAdapter(taskNotesTasksPath, context.messageAdapter(ReceiveVaultPathUpdatedEvent)), "FolderWatcher").underlying
//      ),
//      s"FolderWatcherActor_for_tasks"
//    )

    // FIXME: need to list taskNotesTasksPath and create actors, then wait for new ones
    val existing = Files.list(taskNotesTasksPath).toScala(List)


    Tinker.receiveMessage { m=>
      context.actorContext.log.info(s"ignoring message $m")
      Tinker.steadily
//      case ReceiveVaultPathUpdatedEvent(VaultPathAdapter.PathCreatedEvent(path)) =>
//        context.actorContext.log.info("")
//        Tinker.steadily
//      case ReceiveVaultPathUpdatedEvent(VaultPathAdapter.PathModifiedEvent(path)) =>
//        Tinker.steadily
//      case ReceiveVaultPathUpdatedEvent(VaultPathAdapter.PathDeletedEvent(path)) =>
//        Tinker.steadily
    }
  }
}

object TaskNoteActor {
  sealed trait Message

  private final case class NotePing(ping: Ping) extends Message

  //

  private val Subfolder = "TaskNotes/Tasks/"

  /**
   * noteId needs to exist in /TaskNotes/Tasks/
   */
  def apply(noteId: NoteId)(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, NotePing](noteId.id, TinkerColor.random(), "🧹", NotePing, Some(Subfolder)) { (context, noteRef) =>
    noteRef.getDetails() match {
      case Failure(exception) =>
        context.actorContext.log.warn(s"Problem trying to read frontmatter during initialization for ${noteId.asString} in $Subfolder", exception)
        Tinker.ignore

      case Success(None) =>
        context.actorContext.log.warn(s"Note ${noteId.asString} in $Subfolder had no frontmatter")
        Tinker.ignore

      case Success(Some(modelTaskNote)) =>
        implicit val nr: NoteRef = noteRef
        behavior(modelTaskNote)
    }
  }

  private def behavior(modelTaskNote: ModelTaskNote)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case NotePing(NoOp) =>
        noteRef.getDetails() match {
          case Failure(exception) =>
            context.actorContext.log.warn(s"(ignoring) Problem trying to re-read frontmatter for ${noteRef.noteId.asString} in $Subfolder", exception)
            Tinker.steadily

          case Success(None) =>
            context.actorContext.log.warn(s"(ignoring) Note ${noteRef.noteId.asString} in $Subfolder had no frontmatter on re-read")
            Tinker.steadily

          case Success(Some(modelTaskNote)) =>
            context.actorContext.log.info(s"Reloaded task for ${noteRef.noteId.asString}")
            behavior(modelTaskNote)
        }
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    import TaskNote.TestProtocol.modelTaskNoteYamlFormat
    def getDetails(): Try[Option[ModelTaskNote]] = noteRef.readNote()
      .flatMap { note =>
        Try(note.maybeFrontmatter.map(_.parseYaml.convertTo[ModelTaskNote]))
      }
  }

//  private object ConfigSerialization extends DefaultYamlProtocol {
//
//  }
}






// tinkering, not integrated


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
//                            title: String,
                            scheduled: String,
                            status: String, //TaskNoteStatus,
//                            due: LocalDate, // DateTime,
                            priority: String, //TaskNotePriority,
//                            contexts: List[String],
//                            projects: List[String], //FIXME? NoteId
//                            timeEstimate: Int, //minutes
//                            timeEntries: List[TimeEntry],
                            recurrence: RecurrenceWrapper //,
//                            complete_instances: List[LocalDate] // FIXME or ZonedDateTime?
                          ) {
    def recurrenceRule: RecurrenceRule = recurrence.rr
  }

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
    implicit val modelTaskNoteYamlFormat: YamlFormat[ModelTaskNote] = yamlFormat4(ModelTaskNote)
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
    val testYaml = """status: open
                     |priority: high
                     |scheduled: 2025-09-08T18:45
                     |recurrence: FREQ=DAILY
                     |dateCreated: 2025-09-08T15:31:58.102-07:00
                     |dateModified: 2025-09-08T15:31:58.102-07:00
                     |tags:
                     |  - task""".stripMargin

    val yamlValue: YamlValue =
//      ModelYaml.parseYaml
      testYaml.parseYaml

    val modelTaskNote = yamlValue.convertTo[ModelTaskNote]
    println(modelTaskNote)

//    println("---")
//    print(yamlValue.print(new SnakeYamlPrinterTEST()))
//    println("---")

    println(modelTaskNote.recurrence)
    val recurrenceRule: RecurrenceRule = modelTaskNote.recurrenceRule
    val it = recurrenceRule.iterator(DateTime.today())
    val nextDateTime: DateTime = it.nextDateTime() // ew, side effect; stick in a LazyList?
//    nextDateTime.
//    println(nextDateTime)
//    println(LocalDate.parse(nextDateTime.toString, DateTimeFormatter.ofPattern("yyyyMMdd")))

    val inAndOut = modelTaskNote.toYaml.print.parseYaml.convertTo[ModelTaskNote]
//    println(modelTaskNote.recurrence == inAndOut.recurrence)
//    println(modelTaskNote.copy(recurrence = null) == inAndOut.copy(recurrence = null)) // HA... but also damn
//    println(modelTaskNote)
//    println(inAndOut)

    val it2 = inAndOut.recurrenceRule.iterator(DateTime.now())
    val nextDay: DateTime = it2.next()
//    println(nextDay)

//    val nextAsZonedDateTime = TimeUtil.pythonEpocheToZonedDateTime(next.getTimestamp/1000)
//    println(TimeUtil.pythonEpocheToZonedDateTime(next.getTimestamp/1000))
//    println(TimeUtil.pythonEpocheToZonedDateTime(it2.next().getTimestamp/1000))

    //    while (it.hasNext() && rule.isInfinite())
    //    {
    //      val nextInstance = it.nextDateTime();
    //      // do something with nextInstance
    //    }
    // adapted from https://stackoverflow.com/questions/43319008/how-to-parse-an-ical-rrule-in-java


    val scheduled: LocalDateTime = LocalDateTime.parse(
      modelTaskNote.scheduled.replace("T", " "),
      TimeUtil.YearMonthDaySpaceTimeFormatter
    )

//    println(s"month ${nextDay.getMonth} from $nextDay (${nextDay.getDayOfMonth})")
    val nextLocalDate = LocalDate.of(nextDay.getYear, nextDay.getMonth+1, nextDay.getDayOfMonth)
    val nextZonedDateTime = ZonedDateTime.of(nextLocalDate, scheduled.toLocalTime, ZoneId.systemDefault())
    println(nextZonedDateTime)

    println(nextZonedDateTime.plusDays(recurrenceRule.getInterval))

    //    val optimism: LocalDateTime = scheduled.withYear(nextAsZonedDateTime.getYear).withMonth(nextAsZonedDateTime.getMonthValue).withDayOfMonth(nextAsZonedDateTime.getDayOfMonth)
    //    println(optimism)
    //
    //    val x = ZonedDateTime.of(optimism, ZoneId.systemDefault())

  }
}
