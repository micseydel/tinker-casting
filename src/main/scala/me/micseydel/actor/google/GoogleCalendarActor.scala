package me.micseydel.actor.google

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.DateTime
import com.google.api.services.calendar.Calendar
import com.google.api.services.calendar.model.Event
import me.micseydel.actor.notifications.NotificationCenterManager.{NewNotification, Notification, NotificationId}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{Operator, Tinker, TinkerContext}

import java.time.ZonedDateTime
import scala.jdk.CollectionConverters.CollectionHasAsScala

object GoogleCalendarActor {
  sealed trait Message


  private case class ItsMidnight(midnight: ZonedDateTime) extends Message

  def apply(credential: Credential)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val service: Calendar = TinkerGoogleCalendarService.createCalendarService(credential)

    context.system.operator !! Operator.SubscribeMidnight(context.messageAdapter(ItsMidnight))

    Tinker.receiveMessage {
      case ItsMidnight(midnight) =>
        context.actorContext.log.info(s"Detected midnight: $midnight")
        val start = midnight
        val end = start.plusDays(1)
        val events = service.getTodaysEvents(
          new DateTime(start.toInstant.toEpochMilli),
          new DateTime(end.toInstant.toEpochMilli)
        )
        context.system.notifier !! NewNotification(Notification(midnight, s"There are ${events.size} Google Calendar events today", None, NotificationId("todayscalendar"), Nil))
        Tinker.steadily
    }
  }

  //

  private implicit class RichService(val service: Calendar) extends AnyVal {
    def getTodaysEvents(from: DateTime, to: DateTime): List[Event] = {
      val events = service.events
        .list("primary")
        .setMaxResults(10)
        .setTimeMin(from)
        .setTimeMax(to)
        .setOrderBy("startTime")
        .setSingleEvents(true)
        .execute

      val items = events.getItems
      items.asScala.toList
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
      .setApplicationName("Calendar Actor Service") // FIXME: APPLICATION_NAME
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
