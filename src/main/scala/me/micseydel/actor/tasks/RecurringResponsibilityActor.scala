package me.micseydel.actor.tasks

import cats.data.Validated
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.*
import me.micseydel.actor.tasks.RecurringResponsibilityActorDocument.{Document, FrontmatterConfig, NtfyIfLate}
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.cast.chronicler.ChroniclerMOC.AutomaticallyIntegrated
import me.micseydel.dsl.cast.{Gossiper, TimeKeeper}
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.model.{NotedTranscription, TranscriptionCapture, WhisperResult}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.vault.{Note, NoteId}
import me.micseydel.{Common, NoOp}
import net.jcazevedo.moultingyaml.*
import org.slf4j.Logger

import java.io.FileNotFoundException
import java.security.MessageDigest
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalTime, ZonedDateTime}
import scala.concurrent.duration.{DurationInt, DurationLong}
import scala.util.{Failure, Success, Try}


object RecurringResponsibilityActor {
  sealed trait Message

  private final case class NotePing(ping: Ping) extends Message

  private case class ReceiveTranscription(transcription: NotedTranscription) extends Message

  private final case object MidnightForNextNotificationDayTimer extends Message
  private final case object TimeToNtfy extends Message

  def apply(noteId: String, manager: SpiritRef[RecurringResponsibilityManager.Track])(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, NotePing](noteId, TinkerColor.rgb(0, 50, 100), "🔥", NotePing, Some("_actor_notes") /*FIXME*/) { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context
      implicit val c: TinkerClock = context.system.clock
      val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

      implicit val l: Logger = context.actorContext.log
      l.debug("setting up")

      noteRef.getDocument() match {
        case Success(d@Document(config, markedAsDone, _)) =>
          if (config.voice_completion.nonEmpty) {
            context.actorContext.log.info("Subscribing to Gossiper")
            Tinker.userExtension.gossiper !! Gossiper.SubscribeAccurate(context.messageAdapter(ReceiveTranscription))
          }

          // if this happens to be in the past, it'll trigger immediately
          val nextTriggerDay: LocalDate = (d.latestEntry match {
            case Some(latestEntry) => latestEntry
            case None => context.system.clock.today()
          }).plusDays(config.interval_days)

          manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, nextTriggerDay)

          val nagDaily = config.nag_daily.getOrElse(false)
          if (nagDaily) {
            context.actorContext.log.info("Daily nagging configured, setting timer for daily at midnight")
            val now = ZonedDateTime.now()
            val secondsToMidnight = (now.toEpochSecond - nextTriggerDay.toEpochSecond(context.system.clock.now().toLocalTime, now.getOffset))
            timeKeeper !! TimeKeeper.RemindMeEvery(24.hours, secondsToMidnight.seconds, context.self, MidnightForNextNotificationDayTimer, Some(MidnightForNextNotificationDayTimer))
          } else {
            context.actorContext.log.info("Setting timer for midnight")
            timeKeeper !! TimeKeeper.RemindMeAt(nextTriggerDay, context.self, MidnightForNextNotificationDayTimer, Some(MidnightForNextNotificationDayTimer))
          }

          if (markedAsDone) {
            val forDay = context.system.clock.today() // FIXME: we can't ack without knowing the day the NoteId was, and this will be mostly right for now 😕 (the race condition is unlikely!)
            Tinker.userExtension.chronicler !! Chronicler.ListenerAcknowledgement(noteRef.noteId, forDay, context.system.clock.now(), "marked as done", Some(AutomaticallyIntegrated))
            val today = context.system.clock.today()
            noteRef.prepend(today, Some(today.plusDays(config.interval_days)), None) match {
              case Failure(exception) => context.actorContext.log.error("Something went wrong prepending", exception)
              case Success(NoOp) =>
            }
          }

          behavior(config)(Tinker, noteRef, timeKeeper, manager)

        case Failure(exception) =>
          context.actorContext.log.error("Something went wrong", exception)
          Tinker.ignore
      }
    }

  private def behavior(config: FrontmatterConfig)(implicit Tinker: EnhancedTinker[MyCentralCast], noteRef: NoteRef, timeKeeper: SpiritRef[TimeKeeper.Message], manager: SpiritRef[RecurringResponsibilityManager.Track]): Ability[Message] = Tinker.setup { context =>
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

          case Success(doc@Document(config, markedAsDone, _)) =>
            val Today = context.system.clock.today()
            val result: Try[NoOp.type] = (markedAsDone, doc.latestEntry) match {
              case (false, None) =>
                // start the interval from today
                val triggerDay = Today.plusDays(config.interval_days)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, triggerDay)
                context.actorContext.log.info(s"Scheduling trigger day $triggerDay")
                timeKeeper !! TimeKeeper.RemindMeAt(triggerDay, context.self, MidnightForNextNotificationDayTimer, Some(MidnightForNextNotificationDayTimer))
                Success(NoOp)

              case (false, Some(Today)) =>
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, Today.plusDays(config.interval_days))
                Success(NoOp) // just ignore this

              case (false, Some(latestEntry)) =>
                val triggerDay = latestEntry.plusDays(config.interval_days)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, triggerDay)
                if (triggerDay.isBefore(Today)) {
                  // it should have already triggered
                  context.actorContext.log.warn(s"Trigger day $triggerDay is before today ($Today) so sending TimerUp to self")
                  context.self !! MidnightForNextNotificationDayTimer
                } else {
                  val triggerInDays = TimeUtil.daysBetween(Today, triggerDay).toInt.days
                  context.actorContext.log.info(s"Will trigger in $triggerInDays days (trigger day $triggerDay, latest entry $latestEntry)")
                  timeKeeper !! TimeKeeper.RemindMeAt(triggerDay, context.self, MidnightForNextNotificationDayTimer, Some(MidnightForNextNotificationDayTimer))
                }
                Success(NoOp)

              case (true, Some(Today)) =>
                val notificationId = notificationIdForNoteId(noteRef.noteId)
                context.system.notifier !! CompleteNotification(notificationId)
                timeKeeper !! TimeKeeper.Cancel(Some(TimeToNtfy)) // fire and forget just in case

                context.actorContext.log.info("Button was pushed but there's already an entry for today, clearing button")
                val triggerDay = Today.plusDays(config.interval_days)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, triggerDay)
                noteRef.resetButton(Some(triggerDay))

              case (true, _) =>
                val notificationId = notificationIdForNoteId(noteRef.noteId)
                context.system.notifier !! CompleteNotification(notificationId)
                timeKeeper !! TimeKeeper.Cancel(Some(TimeToNtfy)) // fire and forget just in case

                val nextTrigger = Today.plusDays(config.interval_days)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, nextTrigger)
                timeKeeper !! TimeKeeper.RemindMeAt(nextTrigger, context.self, MidnightForNextNotificationDayTimer, Some(MidnightForNextNotificationDayTimer))
                context.actorContext.log.info(s"Prepending today ($Today) and setting timer for $nextTrigger")
                val forDay = context.system.clock.today()
                Tinker.userExtension.chronicler !! Chronicler.ListenerAcknowledgement(noteRef.noteId, forDay, context.system.clock.now(), "marked as done", Some(AutomaticallyIntegrated))
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

        config.voice_completion match {
          case None => context.actorContext.log.warn("No voice completion config, should not have subscribed to Gossiper and should not have received this message! Bug!")
          case Some(voiceCompletion) =>
            context.actorContext.log.debug(s"Using $voiceCompletion to check...")
            if (loweredText.contains("mark") && (loweredText.contains("as completed") || loweredText.contains("is completed") || loweredText.contains("as done"))) {
              if (voiceCompletion.matches(loweredText)) {
                val notificationId = notificationIdForNoteId(noteRef.noteId)
                context.system.notifier !! CompleteNotification(notificationId)

                val today = context.system.clock.today()
                val nextTrigger = context.system.clock.today().plusDays(config.interval_days)
                manager !! RecurringResponsibilityManager.Track(noteRef.noteId.id, nextTrigger)
                timeKeeper !! TimeKeeper.RemindMeAt(nextTrigger, context.self, MidnightForNextNotificationDayTimer, Some(MidnightForNextNotificationDayTimer))
                context.actorContext.log.info(s"Prepending today ($today), setting timer for $nextTrigger, and ack'ing as done ${noteRef.noteId}")
                noteRef.prepend(today, Some(nextTrigger), Some(noteId))
                val ack = Chronicler.ListenerAcknowledgement(noteId, captureTime.toLocalDate, context.system.clock.now(), "marked as done", Some(AutomaticallyIntegrated))
                Tinker.userExtension.chronicler !! ack
              } else {
                context.actorContext.log.info("Mark as completion request detected, but not a match")
              }
            } else {
              context.actorContext.log.debug("voice note not intended to mark something as completed")
            }
        }


        Tinker.steadily

      case MidnightForNextNotificationDayTimer =>
        val notificationId: String = notificationIdForNoteId(noteRef.noteId)
        context.actorContext.log.info(s"TimerUp, sending notification $notificationId")

        noteRef.getDocument() match {
          case Failure(exception) =>
            context.actorContext.log.warn(s"Failed to get document from disk", exception)
            Tinker.steadily

          case Success(d@Document(config, markedAsDone, _)) =>
            val eligibleSince: LocalDate = (d.latestEntry match {
              case Some(latestEntry) => latestEntry
              case None => context.system.clock.today()
            }).plusDays(config.interval_days)

            context.system.notifier !! NewNotification(Notification(
              context.system.clock.now(),
              s"${noteRef.noteId} eligible since $eligibleSince",
              None,
              NotificationId(notificationId),
              Nil, // FIXME: specify side-effects in the yaml?
              None
            ))

            config.ntfy_if_late.foreach { case ntfyif@NtfyIfLate(_, _, _) =>
              ntfyif.nextTrigger(context.system.clock.now()) match {
                case Validated.Valid(at: ZonedDateTime) =>
                  context.actorContext.log.warn(s"[CANARY] Scheduling ntfy at $at")
                  timeKeeper !! TimeKeeper.RemindMeAt(at, context.self, TimeToNtfy, Some(TimeToNtfy))
                case Validated.Invalid(e) =>
                  context.actorContext.log.warn(s"[CANARY] ntfy config was present but failed to get next trigger: $e")
              }
            }

            behavior(config)
        }

      case TimeToNtfy =>
        config.ntfy_if_late match {
          case Some(NtfyIfLate(channel, _, message)) =>
            context.actorContext.log.warn("[CANARY] Sending push notification!")
            val sideEffect = PushNotification(channel, message)
            context.system.notifier !! NotificationCenterManager.JustSideEffect(sideEffect)
          case None =>
            context.actorContext.log.warn("TimeToNtfy but missing config for it; ignoring")
        }
        Tinker.steadily
    }
  }

  //

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
      noteRef.readNote().flatMap { case Note(markdown, frontmatter) =>
        import RecurringResponsibilityActorDocument.YamlProtocol.configYamlFormat
        val parsedConfig = Try(frontmatter.get.parseYaml.convertTo[FrontmatterConfig]) // FIXME: .get

        parsedConfig.flatMap(config => Try {
          val lines = markdown.split("\n").filterNot(_.startsWith(" "))

          // FIXME: how best to check for lines.head containing "mark as complete"? just assuming for now
          val linesAfterDone: List[String] = lines.toList.drop(1)

          Document(config, markdown.startsWith("- [x]"), linesAfterDone.map(_.drop(2)))
        })
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

object RecurringResponsibilityActorDocument {
  case class FrontmatterConfig(interval_days: Int, voice_completion: Option[VoiceCompletion], nag_daily: Option[Boolean], ntfy_if_late: Option[NtfyIfLate])

  case class Document(config: FrontmatterConfig, markedAsDone: Boolean, itemsAfterDone: List[String]) {
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
      ).map(LocalDate.parse) // FIXME: parsing should happen via a Try-wrapped utility!
    }
  }

  case class VoiceCompletion(config: List[List[String]]) {
    def matches(loweredText: String): Boolean =
      config.exists(sublist => sublist.forall(s => loweredText.contains(s)))
  }

  /**
   * @param ifNotDoneBy e.g. 08:00:00-05:00
   */
  case class NtfyIfLate(channel: String, ifNotDoneBy: String, message: String) {
    def nextTrigger(now: ZonedDateTime): Validated[String, ZonedDateTime] = {
      val today = now.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
      val stringToParse = s"${today}T$ifNotDoneBy"
      Try(ZonedDateTime.parse(stringToParse)) match {
        case Failure(exception) =>
          s"Something went wrong with `$ifNotDoneBy`: ${Common.getStackTraceString(exception)}".invalid

        case Success(value) =>
          (if (value.isBefore(now)) {
            value.plusDays(1)
          } else {
            value
          }).valid
      }
    }
  }

  //

  object YamlProtocol extends DefaultYamlProtocol {
    implicit val voiceCompletionYamlFormat: YamlFormat[VoiceCompletion] = yamlFormat1(VoiceCompletion)
    implicit val ntfyIfLateYamlFormat: YamlFormat[NtfyIfLate] = yamlFormat3(NtfyIfLate)
    implicit val configYamlFormat: YamlFormat[FrontmatterConfig] = yamlFormat4(FrontmatterConfig)
  }
}
