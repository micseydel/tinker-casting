package me.micseydel.actor

import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.Yellow
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerContext, Tinkerer}
import me.micseydel.model.{NotedTranscription, TranscriptionCapture, WhisperResult, WhisperResultContent, WhisperSegment}
import me.micseydel.util.MarkdownUtil
import me.micseydel.util.StringImplicits.RichString
import me.micseydel.vault.persistence.{NoteRef, TypedJsonRef}
import me.micseydel.vault.{HeadingId, LinkIdJsonProtocol, NoteId, SpiritId, VaultKeeper}
import spray.json.{DefaultJsonProtocol, JsonFormat, RootJsonFormat}

import java.io.FileNotFoundException
import java.time.ZonedDateTime
import scala.util.{Failure, Success}

object RemindMeListenerActor {
  val SpiritId: SpiritId = me.micseydel.vault.SpiritId(this.getClass)

  sealed trait Message

  case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message

  case class MarkAsDone(notificationId: NotificationId) extends Message

  private val NoteName = "Reminders"
  private val JsonName = "reminders_tinkering"

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message](NoteName, Yellow, "⚠️") { (context, noteRef) =>
    Tinker.withTypedJson(JsonName, StateJsonProtocol.stateFormat) { jsonRef =>
        implicit val c: TinkerContext[_] = context
        context.actorContext.log.info(s"Starting RemindMeListenerActor(note=$NoteName, json=$JsonName): sending Gossiper.SubscribeAccurate")

        context.system.gossiper !! Gossiper.SubscribeAccurate(context.messageAdapter(TranscriptionEvent))
        context.system.notifier !! NotificationCenterManager.RegisterReplyTo(context.messageAdapter(MarkAsDone), SpiritId)

        behavior(noteRef, jsonRef)
    }
  }

  private def behavior(noteRef: NoteRef, jsonRef: TypedJsonRef[State])(implicit Tinker: Tinker): Ability[Message] = Tinkerer(Yellow, "⚠️").receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    val replyTo = Some(SpiritId)
    message match {
      case TranscriptionEvent(NotedTranscription(TranscriptionCapture(whisperResult, captureTime), noteId, _)) =>
        context.actorContext.log.info(s"Received $noteId")
        whisperResult match {
          case WhisperResult(WhisperResultContent(text, segments), meta) if isAMatch(text) =>
            val reminder = Reminder(captureTime, noteId, text)
            val state = jsonRef.readState()

            val (thereWasAnUpdate, latestState) = state.integrate(reminder)
            if (thereWasAnUpdate) {
              context.system.chronicler !! Chronicler.ListenerAcknowledgement(noteId, context.system.clock.now(), "reminder", Some(AutomaticallyIntegrated))

              context.actorContext.log.info(s"State updated! Writing to JSON and note")
              jsonRef.setState(latestState)

              val blockId = captureTime.toEpochSecond.toString

              val (noteIdToUse, textToUse) = if (text.wordCount > 60) {
                val detections = segments.zipWithIndex.collect {
                  case (WhisperSegment(start, text), index) if isAMatch(text) =>
                    // start is float, wtf? a JS conversion thing maybe?
                    (text, s"seg-${meta.model}-$index")
                }

                detections match {
                  case Nil => throw new RuntimeException("isAMatch was incorrect or unused")
                  case List((blockText, reminderBlockId)) =>
                    val notification = Notification(captureTime, blockText, maybeRef = Some(noteId), NotificationId(blockId), sideEffects = Nil, replyTo)
                    context.system.notifier !! NotificationCenterManager.NewNotification(notification)
                    (HeadingId(reminderBlockId, noteId), blockText)
                  case _ =>
                    val notification = Notification(captureTime, text, maybeRef = Some(noteId), NotificationId(blockId), sideEffects = Nil, replyTo)
                    context.system.notifier !! NotificationCenterManager.NewNotification(notification)
                    (noteId, text)
                }
              } else {
                val notification = Notification(captureTime, text, maybeRef = Some(noteId), NotificationId(blockId), sideEffects = Nil, replyTo)
                context.system.notifier !! NotificationCenterManager.NewNotification(notification)
                (noteId, text)
              }

              noteRef.appendOrThrow(MarkdownUtil.listLineWithTimestampAndRef(captureTime, textToUse, noteIdToUse, blockId = Some(blockId)))
              context.actorContext.log.info("Updated JSON and note")

            } else {
              context.actorContext.log.info(s"New reminder appears to be a duplicate, ignoring; $reminder was in received reminders ${state.receivedReminders}")
            }

            Tinker.steadily

          case WhisperResult(WhisperResultContent(text, _), _) =>
            context.actorContext.log.debug("trigger phrase was not detected, ignoring")
            Tinker.steadily
        }

      case MarkAsDone(notificationId) =>
        context.actorContext.log.info(s"Marking $notificationId as done")
        noteRef.updateMarkdown(MarkdownUtil.removeLinesEndingWithBlockId(notificationId.id, _)) match {
          case Failure(exception) => context.actorContext.log.error(s"Failed to update markdown", exception)
          case Success(note) => context.actorContext.log.debug(s"MARKDOWN ${note.markdown}")
        }
        Tinker.steadily
    }
  }

  // state

  // basically just for idempotence right now
  private case class State(receivedReminders: List[Reminder]) {
    /**
     * @return (was_updated, latest_state)
     */
    def integrate(reminder: Reminder): (Boolean, State) = {
      if (receivedReminders.contains(reminder)) {
        (false, this)
      } else {
        (true, this.copy(reminder :: receivedReminders))
      }
    }
  }

  private case class Reminder(
                               capturedAt: ZonedDateTime,
                               noteId: NoteId,
                               text: String
                             )

  // persistence

  private case object StateJsonProtocol extends DefaultJsonProtocol {

    import me.micseydel.Common.ZonedDateTimeJsonFormat

    implicit val linkIdFormat: JsonFormat[NoteId] = LinkIdJsonProtocol.noteIdFormat

    implicit val reminderListFormat: RootJsonFormat[List[Reminder]] = listFormat(jsonFormat3(Reminder))

    implicit val stateFormat: RootJsonFormat[State] = jsonFormat1(State)
  }

  // util

  private def isAMatch(text: String): Boolean = {
    text.toLowerCase.contains("remind me") || text.toLowerCase.contains("set a reminder")
  }

  private implicit class RichState(val jsonRef: TypedJsonRef[State]) extends AnyVal {
    def readState(): State = {
      jsonRef.read() match {
        case Success(startState) =>
          startState

        case Failure(_: FileNotFoundException) =>
          State(List())

        case Failure(exception) =>
          throw exception
      }
    }

    def setState(state: State): Unit = {
      jsonRef.set(state) match {
        case Failure(exception) =>
          throw exception
        case Success(_) =>
      }
    }
  }
}
