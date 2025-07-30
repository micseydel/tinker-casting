package me.micseydel.actor.tasks

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager.{NewNotification, Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerClock, TinkerColor, TinkerContext}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.NoteRef

import java.io.FileNotFoundException
import java.security.MessageDigest
import java.time.LocalDate
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}


object RecurringResponsibilityActor {
  sealed trait Message

  private final case class NotePing(ping: Ping) extends Message

  private final case object TimerUp extends Message

  def apply(noteId: String, manager: SpiritRef[RecurringResponsibilityManager.Track])(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, NotePing](noteId, TinkerColor.rgb(0, 50, 100), "🔥", NotePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context
      val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

      context.self !! NotePing(NoOp) // behavior is no different on startup as when the note is updated

      behavior()(Tinker, noteRef, timeKeeper, manager)
    }

  private def behavior()(implicit Tinker: Tinker, noteRef: NoteRef, timeKeeper: SpiritRef[TimeKeeper.Message], manager: SpiritRef[RecurringResponsibilityManager.Track]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context
    implicit val c: TinkerClock = context.system.clock
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
                val triggerDay = Today.plusDays(intervalDays)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, triggerDay)
                context.actorContext.log.info(s"Scheduling trigger day $triggerDay")
                timeKeeper !! TimeKeeper.RemindMeAt(triggerDay, context.self, TimerUp, Some(TimerUp))
                Success(NoOp)

              case (false, Some(Today)) =>
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, Today.plusDays(intervalDays))
                Success(NoOp) // just ignore this

              case (false, Some(latestEntry)) =>
                val triggerDay = latestEntry.plusDays(intervalDays)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, triggerDay)
                if (triggerDay.isBefore(Today)) {
                  // it should have already triggered
                  context.actorContext.log.warn(s"Trigger day $triggerDay is before today ($Today) so sending TimerUp to self")
                  context.self !! TimerUp
                } else {
                  val triggerInDays = TimeUtil.daysBetween(Today, triggerDay).toInt.days
                  context.actorContext.log.info(s"Will trigger in $triggerInDays days (trigger day $triggerDay, latest entry $latestEntry)")
                  timeKeeper !! TimeKeeper.RemindMeAt(triggerDay, context.self, TimerUp, Some(TimerUp))
                }
                Success(NoOp)

              case (true, Some(Today)) =>
                context.actorContext.log.info("Button was pushed but there's already an entry for today, clearing button")
                val triggerDay = Today.plusDays(intervalDays)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, triggerDay)
                noteRef.resetButton(Some(triggerDay))

              case (true, _) =>
                val nextTrigger = Today.plusDays(intervalDays)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, nextTrigger)
                timeKeeper !! TimeKeeper.RemindMeAt(nextTrigger, context.self, TimerUp, Some(TimerUp))
                context.actorContext.log.info(s"Prepending today ($Today) and setting timer for $nextTrigger")
                noteRef.prepend(Today, Some(nextTrigger))

            }) match {
              case Failure(exception) => context.actorContext.log.error("Something went wrong updating Markdown", exception)
              case Success(_) =>
            }
        }

        Tinker.steadily

      case TimerUp =>
        val notificationId = MessageDigest.getInstance("SHA-256")
          .digest(noteRef.noteId.id.getBytes("UTF-8"))
          .take(7)
          .map("%02x".format(_)).mkString
        context.actorContext.log.info(s"TimerUp, sending notification $notificationId")

        context.system.notifier !! NewNotification(Notification(
          context.system.clock.now(),
          s"${noteRef.noteId} eligible since ${context.system.clock.today()}",
          None,
          NotificationId(notificationId),
          Nil, // FIXME: specify side-effects in the yaml?
          None
        ))

        // FIXME: reschedule to badger once a day? configure?

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
    def resetButton(nextTrigger: Option[LocalDate]): Try[NoOp.type] = {
      setDocumentMarkdown(None, nextTrigger)
    }

    def prepend(day: LocalDate, nextTrigger: Option[LocalDate]): Try[NoOp.type] = {
      val itemToPrepend = s"[[${noteRef.noteId.id} ($day)]]"
      setDocumentMarkdown(Some(itemToPrepend), nextTrigger)
    }

    private def setDocumentMarkdown(prependWith: Option[String], nextTrigger: Option[LocalDate]): Try[NoOp.type] = {
      getDocument().flatMap { document =>
        val lines = List(
          List("- [ ] Mark as done"),
          nextTrigger.map(_.toString).toList.map(day => s"    - Next trigger: [[$day]]"),
          prependWith.toList.map("- " + _),
          document.itemsAfterDone.map("- " + _)
        ).flatten

        val newMarkdown = lines.mkString("\n")
        noteRef.setMarkdown(newMarkdown)
      }
    }

    def getDocument(): Try[Document] = {
      noteRef.readNote().flatMap { note =>
        // FIXME: better error handling
        note.yamlFrontMatter.map(_("interval_days").asInstanceOf[Int]).map { intervalDays =>
          // ignore any lines that are indented (comments
          val lines = note.markdown.split("\n").filterNot(_.startsWith(" "))

          // FIXME: how best to check for lines.head containing "mark as complete"? just assuming for now
          val linesAfterDone: List[String] = lines.toList.drop(1)

          Document(
            intervalDays,
            note.markdown.startsWith("- [x]"),
            linesAfterDone.map(_.drop(2)) // drop the Markdown list characters
          )
        }
      }
    }
  }
}
