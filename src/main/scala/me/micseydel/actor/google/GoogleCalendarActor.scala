package me.micseydel.actor.google

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.DateTime
import com.google.api.services.calendar.Calendar
import com.google.api.services.calendar.model.Event
import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.google.GoogleAuthManager.GoogleApplicationName
import me.micseydel.actor.notifications.NotificationCenterManager.{NewNotification, Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TypedMqtt.MqttMessage
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.*
import me.micseydel.vault.persistence.NoteRef
import spray.json.*
import spray.json.DefaultJsonProtocol.{StringJsonFormat, listFormat}

import java.time.ZonedDateTime
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

object GoogleCalendarActor {
  sealed trait Message

  private case class ReceiveMqtt(mqttMessage: TypedMqtt.MqttMessage) extends Message

  private case class ItsMidnight(midnight: ZonedDateTime) extends Message

  private case class ReceiveEventsResult(result: Try[List[Event]]) extends Message

  private case class ReceivePing(ping: Ping) extends Message

  //

  val Topic = "[[Google Calendar]]"

  def apply(credential: Credential)(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, ReceivePing]("Google Calendar Tinkering", TinkerColor.random(), "🗓️", ReceivePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[?] = context

      context.system.mqtt ! TypedMqtt.Subscribe(Topic, context.messageAdapter(ReceiveMqtt).underlying)

      val service: Calendar = TinkerGoogleCalendarService.createCalendarService(credential)

      context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ItsMidnight))

      noteRef.setMarkdown("- [ ] Click to fetch today's calendar events\n") match {
        case Failure(ex) => context.actorContext.log.error("failed to set markdown", ex)
        case Success(NoOp) =>
      }

      implicit val nr: NoteRef = noteRef

      behavior(service)
    }

  private def behavior(service: Calendar)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    implicit val ex: ExecutionContextExecutorService = context.system.httpExecutionContext

    Tinker.receiveMessage {
      case ItsMidnight(midnight) =>
        context.actorContext.log.info(s"Detected midnight: $midnight")
        context.pipeToSelf(Future(service.getDaysEvents(midnight)))(ReceiveEventsResult)
        Tinker.steadily

      case ReceiveEventsResult(result) =>
        result match {
          case Failure(exception) => context.actorContext.log.warn("Failed to get calendar", exception)
          case Success(events) =>
            if (events.isEmpty) {
              context.actorContext.log.warn(s"No events!")
            } else {
              val details: String = events
                .map { event =>

                  val lines = List(
                    s"## ${event.getSummary}\n",
                    "```json",
                    event.toPrettyString,
                    "```\n"
                  )

                  Option(event.getRecurringEventId) match {
                    case Some(recurringEventId) =>
                      // FIXME hack, this should probably be wrapped in a future and everything
                      val series = service.events.get("primary", recurringEventId).execute()
                      (lines ++ List(
                        "### Series",
                        "```json",
                        series.toPrettyString,
                        "```\n"
                      )).mkString("\n")
                    case None =>
                      lines.mkString("\n")
                  }
                }
                .mkString("\n")

              noteRef.setMarkdown(s"- [ ] Click to fetch today's calendar events\n\n$details") match {
                case Failure(ex) => context.actorContext.log.error("failed to set markdown", ex)
                case Success(NoOp) =>
              }

              val msg = s"There are ${events.size} Google Calendar events today"
              val now = context.system.clock.now()
              context.system.notifier !! NewNotification(Notification(now, msg, None, NotificationId(s"todayscalendar-${now.toLocalDate}"), Nil))
            }
        }

        Tinker.steadily

      case ReceivePing(NoOp) =>
        if (noteRef.checkBoxIsChecked()) {
          val midnight = context.system.clock.now().withHour(0).withMinute(0)
          context.pipeToSelf(Future(service.getDaysEvents(midnight)))(ReceiveEventsResult)
        }

        Tinker.steadily

      case ReceiveMqtt(MqttMessage(Topic, payload)) =>
        Try(new String(payload).parseJson.asJsObject.getFields("type", "replyTo") match {
          case Seq(JsString("get_two_weeks"), JsString(replyTo)) =>
            val start = context.system.clock.now()

            // FIXME magic numbers
            val end = start.plusDays(14)

            // FIXME: this does I/O!
            val events = service.getEvents(start, end, 150)

            val json = JsObject(
              "type" -> JsString("google_calendar_events"),
              "events" -> JsArray(events.map(_.toString.parseJson).toVector),
              // FIXME: grab the recurring events while at it
            )

            context.system.mqtt ! TypedMqtt.Publish(replyTo, json.compactPrint.getBytes)
          case other =>
            context.actorContext.log.warn(s"""Expected ["get_two_weeks", replyTo] but got $other""")
        }) match {
          case Failure(exception) => context.actorContext.log.warn("extracting payload failed", exception)
          case Success(_) =>
        }

        Tinker.steadily

      case ReceiveMqtt(MqttMessage(unexpectedTopic, payload)) =>
        context.actorContext.log.warn(s"Received ${payload.length} bytes on unexpected topic $unexpectedTopic")
        Tinker.steadily
    }
  }

  //

  private implicit class RichService(val service: Calendar) extends AnyVal {
    def getEvents(from: ZonedDateTime, to: ZonedDateTime, maxResults: Int): List[Event] = {
      getEvents(new DateTime(from.toInstant.toEpochMilli), new DateTime(to.toInstant.toEpochMilli), maxResults)
    }

    def getEvents(from: DateTime, to: DateTime, maxResults: Int = 10): List[Event] = {
      val events = service.events
        .list("primary")
        .setMaxResults(maxResults)
        .setTimeMin(from)
        .setTimeMax(to)
        .setOrderBy("startTime")
        .setSingleEvents(true)
        .execute

      val items = events.getItems
      items.asScala.toList
    }

    def getDaysEvents(day: ZonedDateTime): List[Event] = {
      service.getEvents(
        new DateTime(day.toInstant.toEpochMilli),
        new DateTime(day.plusDays(1).toInstant.toEpochMilli)
      )
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown().map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }
}

private object TinkerGoogleCalendarService {
  def createCalendarService(credential: Credential): Calendar = {
    new Calendar.Builder(
      GoogleNetHttpTransport.newTrustedTransport(),
      GsonFactory.getDefaultInstance,
      credential
    )
      .setApplicationName(GoogleApplicationName)
      .build()
  }

  //  def getThem(service: Calendar): List[Event] = {
  //    val now = new DateTime(System.currentTimeMillis)
  //    val events = service.events.list("primary").setMaxResults(10).setTimeMin(now).setOrderBy("startTime").setSingleEvents(true).execute
  //    val items = events.getItems
  //    items.asScala.toList
  //  }
}


// FIXME: turn these private methods into a trait so I can have main functions for easy testing outside of actors
//object SimpleTinkerApp {
//  def main(args: Array[String]): Unit = {
//    import GmailActor.RichNoteRef
//
//    createNoteRef(GmailActor.NoteName, "_actor_notes").andThen(_.getDocument()) match {
//      case Validated.Invalid(e) => throw new RuntimeException(e.toString)
//      case Validated.Valid(Document(_, /*gmailConfig,*/ _)) =>
//        val calendar = createCalendarService(gmailConfig)
//        val events = getThem(calendar)
//        println(s"${events.size} events:")
//        events.foreach(println)
//    }
//  }
//
//  private def createNoteRef(noteName: String, subfolder: Option[String] = None): Validated[String, NoteRef] = {
//    val EnvVar = "vaultRoot"
//    sys.env.get(EnvVar) match {
//      case None => s"Missing environment variable: $EnvVar".invalid
//      case Some(rootPath) =>
//        VaultPath(Path.of(rootPath)).map(vaultRoot =>
//          new BasicNoteRef(NoteId(noteName), vaultRoot, subfolder)
//        )
//    }
//  }
//
//  private def createNoteRef(noteName: String, subfolder: String): Validated[String, NoteRef] = {
//    createNoteRef(noteName, Some(subfolder))
//  }
//}
