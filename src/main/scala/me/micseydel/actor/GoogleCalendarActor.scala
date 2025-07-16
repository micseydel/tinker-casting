package me.micseydel.actor

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.DateTime
import com.google.api.services.calendar.Calendar
import com.google.api.services.calendar.model.Event
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability

import scala.jdk.CollectionConverters.CollectionHasAsScala

object GoogleCalendarActor {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.unhandled
  }
}

private object TinkerGoogleCalendarService {
  def createCalendarService(config: GmailConfig): Calendar = {
    val credential = GmailAuth.authenticate(config.credentialsPath, config.tokensPath)
    new Calendar.Builder(
      GoogleNetHttpTransport.newTrustedTransport(),
      GsonFactory.getDefaultInstance,
      credential
    )
      .setApplicationName("Calendar Actor Service") // FIXME: APPLICATION_NAME
      .build()
  }

  def getThem(service: Calendar): List[Event] = {
    // List the next 10 events from the primary calendar.
    val now = new DateTime(System.currentTimeMillis)
    val events = service.events.list("primary").setMaxResults(10).setTimeMin(now).setOrderBy("startTime").setSingleEvents(true).execute
    val items = events.getItems
    items.asScala.toList
  }

  def main(args: Array[String]): Unit = {
    val config = GmailConfig("/Users/micseydel/Downloads/client_secret_499772534240-94r7ttm409fn408rnng7po9r7g87mjgp.apps.googleusercontent.com.json", "/Users/micseydel/.gmail_tokens.json")
    val calendar = createCalendarService(config)

    val events = getThem(calendar)
    println(s"${events.size} events: $events")
  }
}

