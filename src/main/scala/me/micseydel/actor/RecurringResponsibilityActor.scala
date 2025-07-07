package me.micseydel.actor

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager.{NewNotification, Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.util.{MarkdownUtil, TimeUtil}
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}

import java.io.FileNotFoundException
import java.security.MessageDigest
import java.time.LocalDate
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object RecurringResponsibilityManager {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Recurring responsibilities (MOC)", TinkerColor.random(), "⛑️") { (context, noteRef) =>
    noteRef.readListOfWikiLinks() match {
      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"Something went wrong setting up ${noteRef.noteId}: $e")
        Tinker.ignore
      case Validated.Valid(recurringResponsibilities: NonEmptyList[String]) =>
        val specificResponsibilityTrackers = recurringResponsibilities.map { recurringResponsibility =>
          context.actorContext.log.info(s"Casting $recurringResponsibility")
          context.cast(RecurringResponsibilityActor(recurringResponsibility), Common.tryToCleanForActorName(recurringResponsibility))
        }
        Tinker.ignore
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def readListOfWikiLinks(): ValidatedNel[String, NonEmptyList[String]] = {
      noteRef.readMarkdown().map(MarkdownUtil.readListOfWikiLinks) match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(result) =>
          result
      }
    }
  }
}

object RecurringResponsibilityActor {
  sealed trait Message

  private final case class NotePing(ping: Ping) extends Message

  private final case object TimerUp extends Message

  def apply(noteId: String)(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, NotePing](noteId, TinkerColor.rgb(0, 50, 100), "🔥", NotePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context
      val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

      context.self !! NotePing(NoOp) // behavior is no different on startup as when the note is updated

      behavior()(Tinker, noteRef, timeKeeper)
    }

  private def behavior()(implicit Tinker: Tinker, noteRef: NoteRef, timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context
    Tinker.receiveMessage {
      case NotePing(_) =>
        noteRef.getDocument() match {
          case Failure(_: FileNotFoundException) =>
            context.actorContext.log.warn(s"File ${noteRef.noteId} did not exist")
            Tinker.ignore
          case Failure(exception) => throw exception // FIXME

          case Success(doc@Document(intervalDays, markedAsDone, _)) =>
            val Today = context.system.clock.today()
            ((markedAsDone, doc.latestEntry) match {
              case (false, None) =>
                // start the interval from today
                timeKeeper !! TimeKeeper.RemindMeIn(intervalDays.days, context.self, TimerUp, Some(TimerUp))
                Success(NoOp)

              case (false, Some(Today)) =>
                Success(NoOp) // just ignore this

              case (false, Some(latestEntry)) =>
                val triggerDay = latestEntry.plusDays(intervalDays)
                if (triggerDay.isBefore(Today)) {
                  // it should have already triggered
                  context.actorContext.log.warn(s"Trigger day $triggerDay is before today ($Today) so sending TimerUp to self")
                  context.self !! TimerUp
                } else {
                  val triggerInDays = TimeUtil.daysBetween(Today, triggerDay).toInt.days
                  context.actorContext.log.info(s"Will trigger in $triggerInDays days (trigger day $triggerDay, latest entry $latestEntry)")
                  timeKeeper !! TimeKeeper.RemindMeIn(triggerInDays, context.self, TimerUp, Some(TimerUp))
                }
                Success(NoOp)

              case (true, Some(Today)) =>
                noteRef.resetButton()

              case (true, _) =>
                noteRef.prepend(Today)

            }) match {
              case Failure(exception) => context.actorContext.log.error("Something went wrong updating Markdown", exception)
              case Success(_) =>
            }
        }

        Tinker.steadily

      case TimerUp =>
        context.system.notifier !! NewNotification(Notification(
          context.system.clock.now(),
          s"${noteRef.noteId} eligible since ${context.system.clock.today()}",
          None,
          NotificationId(MessageDigest.getInstance("SHA-256")
            .digest(noteRef.noteId.id.getBytes("UTF-8"))
            .take(7)
            .map("%02x".format(_)).mkString),
          Nil, // FIXME: specify side-effects in the yaml?
          None
        ))
        Tinker.steadily
    }
  }

  //

  case class Document(intervalDays: Int, markedAsDone: Boolean, itemsAfterDone: List[String]) {
    def latestEntry: Option[LocalDate] = {
      itemsAfterDone.headOption.map(_.dropRight(3).takeRight(10)).map(LocalDate.parse)
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def resetButton(): Try[NoOp.type] = {
      setDocumentMarkdown(None)
    }

    def prepend(day: LocalDate): Try[NoOp.type] = {
      val itemToPrepend = s"[[${noteRef.noteId.id} ($day)]]"
      setDocumentMarkdown(Some(itemToPrepend))
    }

    private def setDocumentMarkdown(prependWith: Option[String]): Try[NoOp.type]= {
      getDocument().flatMap { document =>
        val newMarkdown = "- [ ] Mark as done\n" +
          (prependWith.toList ::: document.itemsAfterDone) // FIXME: lazy
            .map("- " + _)
            .mkString("\n")
        noteRef.setMarkdown(newMarkdown)
      }
    }

    def getDocument(): Try[Document] = {
      noteRef.readNote().flatMap { note =>
        // FIXME: better error handling
        note.yamlFrontMatter.map(_("interval_days").asInstanceOf[Int]).map { intervalDays =>
          val lines = note.markdown.split("\n")

          // FIXME: how best to check for lines.head containing "mark as complete"? just assuming for now
          val linesAfterDone: List[String] = lines.toList.drop(1)

          Document(
            intervalDays,
            note.markdown.startsWith("- [x]"),
            linesAfterDone.map(_.drop(2)) // drop the list part
          )
        }
      }
    }
  }
}
