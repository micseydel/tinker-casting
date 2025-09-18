package me.micseydel.actor.tasks

import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager.{CompleteNotification, NewNotification, Notification, NotificationId}
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.{Gossiper, TimeKeeper}
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.*
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.model.{NotedTranscription, TranscriptionCapture, WhisperResult}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import org.slf4j.Logger

import java.io.FileNotFoundException
import java.security.MessageDigest
import java.time.LocalDate
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}


object RecurringResponsibilityActor {
  sealed trait Message

  private final case class NotePing(ping: Ping) extends Message

  private case class ReceiveTranscription(transcription: NotedTranscription) extends Message

  private final case object TimerUp extends Message

  def apply(noteId: String, manager: SpiritRef[RecurringResponsibilityManager.Track])(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, NotePing](noteId, TinkerColor.rgb(0, 50, 100), "🔥", NotePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context
      implicit val c: TinkerClock = context.system.clock
      val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

      implicit val l: Logger = context.actorContext.log
      l.debug("setting up")

      noteRef.getDocument() match {
        case Success(d@Document(intervalDays, markedAsDone, _, maybeVoiceCompletion)) =>
          if (maybeVoiceCompletion.nonEmpty) {
            context.actorContext.log.info("Subscribing to Gossiper")
            Tinker.userExtension.gossiper !! Gossiper.SubscribeAccurate(context.messageAdapter(ReceiveTranscription))
          }

          val nextTriggerDay = (d.latestEntry match {
            case Some(latestEntry) => latestEntry
            case None => context.system.clock.today()
          }).plusDays(intervalDays)

          manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, nextTriggerDay)
          timeKeeper !! TimeKeeper.RemindMeAt(nextTriggerDay, context.self, TimerUp, Some(TimerUp))

          if (markedAsDone) {
            Tinker.userExtension.chronicler !! Chronicler.ListenerAcknowledgement(noteRef.noteId, context.system.clock.now(), "marked as done", Some(AutomaticallyIntegrated))
            val today = context.system.clock.today()
            noteRef.prepend(today, Some(today.plusDays(intervalDays)), None) match {
              case Failure(exception) => context.actorContext.log.error("Something went wrong prepending", exception)
              case Success(NoOp) =>
            }
          }

          behavior(intervalDays, maybeVoiceCompletion)(Tinker, noteRef, timeKeeper, manager)

        case Failure(exception) =>
          context.actorContext.log.error("Something went wrong", exception)
          Tinker.ignore
      }
    }

  private def behavior(intervalDays: Int, maybeVoiceCompletion: Option[VoiceCompletion])(implicit Tinker: EnhancedTinker[MyCentralCast], noteRef: NoteRef, timeKeeper: SpiritRef[TimeKeeper.Message], manager: SpiritRef[RecurringResponsibilityManager.Track]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context
    implicit val c: TinkerClock = context.system.clock
    implicit val l: Logger = context.actorContext.log
    l.debug("creating message receiver")
    Tinker.receiveMessage {
      case NotePing(_) =>
        noteRef.getDocument() match {
          case Failure(_: FileNotFoundException) =>
            context.actorContext.log.warn(s"File ${noteRef.noteId} did not exist")
            Tinker.ignore
          case Failure(exception) => throw exception // FIXME

          case Success(doc@Document(intervalDays, markedAsDone, _, _)) =>
            val Today = context.system.clock.today()
            val result: Try[NoOp.type] = (markedAsDone, doc.latestEntry) match {
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
                val notificationId = notificationIdForNoteId(noteRef.noteId)
                context.system.notifier !! CompleteNotification(notificationId)

                context.actorContext.log.info("Button was pushed but there's already an entry for today, clearing button")
                val triggerDay = Today.plusDays(intervalDays)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, triggerDay)
                noteRef.resetButton(Some(triggerDay))

              case (true, _) =>
                val notificationId = notificationIdForNoteId(noteRef.noteId)
                context.system.notifier !! CompleteNotification(notificationId)

                val nextTrigger = Today.plusDays(intervalDays)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, nextTrigger)
                timeKeeper !! TimeKeeper.RemindMeAt(nextTrigger, context.self, TimerUp, Some(TimerUp))
                context.actorContext.log.info(s"Prepending today ($Today) and setting timer for $nextTrigger")
                Tinker.userExtension.chronicler !! Chronicler.ListenerAcknowledgement(noteRef.noteId, context.system.clock.now(), "marked as done", Some(AutomaticallyIntegrated))
                noteRef.prepend(Today, Some(nextTrigger), None)
            }

            result match {
              case Failure(exception) => context.actorContext.log.error("Something went wrong updating Markdown", exception)
              case Success(_) =>
            }
        }

        Tinker.steadily

      case ReceiveTranscription(NotedTranscription(TranscriptionCapture(WhisperResult(whisperResultContent, whisperResultMetadata), captureTime), noteId)) =>
        context.actorContext.log.info(s"Received transcription $noteId")
        val loweredText = whisperResultContent.text.toLowerCase

        maybeVoiceCompletion match {
          case None => context.actorContext.log.warn("No voice completion config, should not have subscribed to Gossiper and should not have received this message! Bug!")
          case Some(voiceCompletion) =>
            context.actorContext.log.debug(s"Using $voiceCompletion to check...")
            if (loweredText.contains("mark") && (loweredText.contains("as completed") || loweredText.contains("as done"))) {
              if (voiceCompletion.matches(loweredText)) {
                val notificationId = notificationIdForNoteId(noteRef.noteId)
                context.system.notifier !! CompleteNotification(notificationId)

                val today = context.system.clock.today()
                val nextTrigger = context.system.clock.today().plusDays(intervalDays)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, nextTrigger)
                timeKeeper !! TimeKeeper.RemindMeAt(nextTrigger, context.self, TimerUp, Some(TimerUp))
                context.actorContext.log.info(s"Prepending today ($today), setting timer for $nextTrigger, and ack'ing as done ${noteRef.noteId}")
                noteRef.prepend(today, Some(nextTrigger), Some(noteId))
                Tinker.userExtension.chronicler !! Chronicler.ListenerAcknowledgement(noteRef.noteId, context.system.clock.now(), "marked as done", Some(AutomaticallyIntegrated))
              } else {
                context.actorContext.log.info("Mark as completion request detected, but not a match")
              }
            } else {
              context.actorContext.log.debug("voice note not intended to mark something as completed")
            }
        }


        Tinker.steadily

      case TimerUp =>
        val notificationId: String = notificationIdForNoteId(noteRef.noteId)
        context.actorContext.log.info(s"TimerUp, sending notification $notificationId")

        context.system.notifier !! NewNotification(Notification(
          context.system.clock.now(),
          s"${noteRef.noteId} eligible since ${context.system.clock.today()}", // FIXME: this is not correct!
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

  case class VoiceCompletion(config: List[List[String]]) {
    def matches(loweredText: String): Boolean =
      config.exists(sublist => sublist.forall(s => loweredText.contains(s)))
  }

  case class Document(intervalDays: Int, markedAsDone: Boolean, itemsAfterDone: List[String], maybeVoiceCompletion: Option[VoiceCompletion]) {
    def latestEntry: Option[LocalDate] = {
      itemsAfterDone.headOption.flatMap(latest =>
        if (latest.contains(")]] ")) {
          latest.split("\\)]] ").toList match {
            case List(messyWikiLink, ref) =>
              Some(messyWikiLink.takeRight(10))
            case _ =>
              None
          }
        } else {
          Some(latest.dropRight(3).takeRight(10))
        }
      ).map(LocalDate.parse)
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def resetButton(nextTrigger: Option[LocalDate])(implicit log: Logger): Try[NoOp.type] = {
      setDocumentMarkdown(None, nextTrigger)
    }

    def prepend(day: LocalDate, nextTrigger: Option[LocalDate], maybeRef: Option[NoteId])(implicit log: Logger): Try[NoOp.type] = {
      val itemToPrepend = maybeRef match {
        case Some(noteId) =>
          val aliased = noteId.wikiLinkWithAlias("ref")
          s"[[${noteRef.noteId.id} ($day)]] ($aliased)"
        case None => s"[[${noteRef.noteId.id} ($day)]]"
      }
      setDocumentMarkdown(Some(itemToPrepend), nextTrigger)
    }

    private def setDocumentMarkdown(prependWith: Option[String], nextTrigger: Option[LocalDate])(implicit log: Logger): Try[NoOp.type] = {
      getDocument().flatMap { document =>
        val lines = List(
          List("- [ ] Mark as done"),
          nextTrigger.map(_.toString).toList.map(day => s"    - Next trigger: [[$day]]"),
          prependWith.toList.map("- " + _).filterNot(maybeDuplicate => document.itemsAfterDone.headOption.contains(maybeDuplicate)),
          document.itemsAfterDone.map("- " + _)
        ).flatten

        val newMarkdown = lines.mkString("\n")
        noteRef.setMarkdown(newMarkdown)
      }
    }

    def getDocument()(implicit log: Logger): Try[Document] = {
      noteRef.readMarkdownAndFrontmatter().map { case (markdown, frontmatter) =>
        val maybeVoiceCompletion: Option[VoiceCompletion] = frontmatter.get("voice_completion") match {
          case Some(result: java.util.List[_]) =>
            val (passed, failed) = result.asScala.partition(_.isInstanceOf[java.util.List[?]])
            if (failed.nonEmpty) {
              log.warn(s"Expected to find a List[List[String]] but found: $failed")
              None
            } else {
              Try(Some(VoiceCompletion(passed.toList.map(_.asInstanceOf[java.util.List[?]].asScala.toList.map(_.asInstanceOf[String]))))) match {
                case Failure(exception) =>
                  log.warn(s"Deserializing yaml failed - could be because of an intermediary change", exception)
                  None
                case Success(value) => value
              }
            }

          case None => None

          case other =>
            log.warn(s"Expected a List[List[String]] but got $other")
            None
        }

        val intervalDays = frontmatter("interval_days").asInstanceOf[Int]

        // ignore any lines that are indented (comments
        val lines = markdown.split("\n").filterNot(_.startsWith(" "))

        // FIXME: how best to check for lines.head containing "mark as complete"? just assuming for now
        val linesAfterDone: List[String] = lines.toList.drop(1)

        Document(
          intervalDays,
          markdown.startsWith("- [x]"),
          linesAfterDone.map(_.drop(2)), // drop the Markdown list characters
          maybeVoiceCompletion
        )
      }
    }
  }

  private def notificationIdForNoteId(noteId: NoteId): String = {
    MessageDigest.getInstance("SHA-256")
      .digest(noteId.id.getBytes("UTF-8"))
      .take(7)
      .map("%02x".format(_)).mkString
  }
}
