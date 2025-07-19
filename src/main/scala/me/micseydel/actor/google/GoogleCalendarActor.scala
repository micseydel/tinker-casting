package me.micseydel.actor.google

import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.DateTime
import com.google.api.services.calendar.Calendar
import com.google.api.services.calendar.model.Event
import me.micseydel.actor.google.GmailActor.Document
import me.micseydel.actor.google.TinkerGoogleCalendarService.{createCalendarService, getThem}
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.vault.{NoteId, VaultPath}
import me.micseydel.vault.persistence.{BasicNoteRef, NoteRef}

import java.nio.file.Path
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
}


// FIXME: turn these private methods into a trait so I can have main functions for easy testing outside of actors
object SimpleTinkerApp {
  def main(args: Array[String]): Unit = {
    import GmailActor.RichNoteRef

    createNoteRef(GmailActor.NoteName, "_actor_notes").andThen(_.getDocument()) match {
      case Validated.Invalid(e) => throw new RuntimeException(e.toString)
      case Validated.Valid(Document(_, gmailConfig, _)) =>
        val calendar = createCalendarService(gmailConfig)
        val events = getThem(calendar)
        println(s"${events.size} events:")
        events.foreach(println)
    }
  }

  private def createNoteRef(noteName: String, subfolder: Option[String] = None): Validated[String, NoteRef] = {
    val EnvVar = "vaultRoot"
    sys.env.get(EnvVar) match {
      case None => s"Missing environment variable: $EnvVar".invalid
      case Some(rootPath) =>
        VaultPath(Path.of(rootPath)).map(vaultRoot =>
          new BasicNoteRef(NoteId(noteName), vaultRoot, subfolder)
        )
    }
  }

  private def createNoteRef(noteName: String, subfolder: String): Validated[String, NoteRef] = {
    createNoteRef(noteName, Some(subfolder))
  }
}
