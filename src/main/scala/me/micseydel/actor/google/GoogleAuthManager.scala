package me.micseydel.actor.google

import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import com.google.api.client.auth.oauth2.{AuthorizationCodeFlow, Credential}
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.calendar.CalendarScopes
import com.google.api.services.slides.v1.SlidesScopes
import me.micseydel.Common
import me.micseydel.Common.getValidatedStringFromConfig
import me.micseydel.actor.GmailExperimentActor
import me.micseydel.actor.google.GmailActor.AuthExpiredNotificationId
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.app.GoogleSlideUpdater
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import java.io.{File, FileInputStream, InputStreamReader}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success}

object GoogleAuthManager {
  sealed trait Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Google API Configuration", TinkerColor.random(), "🕸️") { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    noteRef.getDocument() match {
      case Validated.Valid(config: GmailConfig) =>
        // FIXME: this should almost certainly be pipeToSelf'd with a Future
        val credential: Credential = GmailAuth.authenticate(config.credentialsPath, config.tokensPath)
        context.system.notifier !! NotificationCenterManager.CompleteNotification(AuthExpiredNotificationId.id)
        behavior(credential)
      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"Failed to get config: $e")
        Tinker.ignore
    }
  }

  private def behavior(credential: Credential)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    val gmail = context.cast(GmailExperimentActor(credential), "GmailExperimentActor")
    val calendar = context.cast(GoogleCalendarActor(credential), "GoogleCalendarActor")
    val slides = context.cast(GoogleSlideUpdater(credential), "GoogleSlideUpdaterActorEXPERIMENT")

    // FIXME: swap the experiment actor above out so I can register with Operator here instead of in the actors
    //   then, update the auth code so that it refreshes automatically
    Tinker.ignore
  }

  //

  val GoogleApplicationName = "Tinker Casting"

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def getDocument(): ValidatedNel[String, GmailConfig] = {
      noteRef.read().flatMap(_.yamlFrontMatter) match {
        case Failure(exception: org.yaml.snakeyaml.scanner.ScannerException) =>
          s"YAML parsing failure ${Common.getStackTraceString(exception)}".invalidNel
        case Failure(exception) =>
          throw exception
        case Success(map) =>
          val validatedTokenPath = getValidatedStringFromConfig(map, "token_path")
          val validatedCredsPath = getValidatedStringFromConfig(map, "creds_path")

          validatedTokenPath.andThen { tokenPath =>
            validatedCredsPath.map { credsPath =>
              GmailConfig(credsPath, tokenPath)
            }
          }
      }
    }
  }
}


private object GmailAuth {
  def authenticate(credentialsPath: String, tokensDir: String): Credential = {
    val httpTransport = GoogleNetHttpTransport.newTrustedTransport()
    val jsonFactory = GsonFactory.getDefaultInstance

    val clientSecrets = GoogleClientSecrets.load(
      jsonFactory,
      new InputStreamReader(new FileInputStream(credentialsPath))
    )

    val flow: AuthorizationCodeFlow = new GoogleAuthorizationCodeFlow.Builder(
      httpTransport,
      jsonFactory,
      clientSecrets,
      List(
        "https://www.googleapis.com/auth/gmail.readonly",
        CalendarScopes.CALENDAR_READONLY,
        CalendarScopes.CALENDAR_ACLS_READONLY,
        CalendarScopes.CALENDAR_CALENDARLIST_READONLY,
        CalendarScopes.CALENDAR_CALENDARS_READONLY,
        CalendarScopes.CALENDAR_EVENTS_OWNED_READONLY,
        CalendarScopes.CALENDAR_EVENTS_PUBLIC_READONLY,
        CalendarScopes.CALENDAR_EVENTS_READONLY,
        CalendarScopes.CALENDAR_SETTINGS_READONLY,
        SlidesScopes.PRESENTATIONS
      ).asJava
    ).setDataStoreFactory(new FileDataStoreFactory(new File(tokensDir)))
      .setAccessType("offline")
      .build()

    // FIXME: use event logger instead?
    val receiver = new LocalServerReceiver.Builder().setPort(8888).build()

    new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
  }
}

