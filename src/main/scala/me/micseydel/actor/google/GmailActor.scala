package me.micseydel.actor.google

import akka.actor.typed.scaladsl.Behaviors
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.implicits.catsSyntaxValidatedId
import com.google.api.client.auth.oauth2.{AuthorizationCodeFlow, Credential}
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.calendar.CalendarScopes
import com.google.api.services.gmail.Gmail
import com.google.api.services.gmail.model.MessagePart
import me.micseydel.Common
import me.micseydel.Common.getValidatedStringFromConfig
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.google.GmailActor.Email
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import java.io.{File, FileInputStream, InputStreamReader}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{ZoneId, ZonedDateTime}
import java.util.Base64
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

object GmailActor {
  sealed trait Message

  case class Subscribe(subscriber: SpiritRef[Seq[Email]]) extends Message

  private case object HeartBeat extends Message

  private case class ReceiveInbox(emails: Try[Seq[Email]]) extends Message

  final case class ReceivePing(ping: Ping) extends Message

  //

  case class Email(sender: String, subject: String, body: String, sentAt: String, groupedHeaders: Map[String, List[String]]) {
    private def tryToGetWestCoastTime: Try[ZonedDateTime] =
      Try(ZonedDateTime.parse(sentAt))
        .map(_.withZoneSameInstant(ZoneId.of("America/Los_Angeles")))

    def getTimeHacky: Try[ZonedDateTime] = {
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
      Try(ZonedDateTime.parse(sentAt.replace("PDT", "-0700").replace("PST", "-0800"), formatter)).recoverWith {
        case _: DateTimeParseException =>
          tryToGetWestCoastTime
      }
    }
  }

  val NoteName = "Gmail Configuration"

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing](NoteName, TinkerColor.random(), "💌", ReceivePing) { case (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context
    implicit val nr: NoteRef = noteRef
    implicit val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

    context.system.operator !! Operator.RegisterGmail(context.self)

    noteRef.getDocument() match {
      case Valid(Document(pollingMinutes, gmailConfig, _)) =>
        context.actorContext.log.info("Starting gmail service")
        implicit val gmailService: Gmail = TinkerGmailService.createGmailService(gmailConfig)
        context.actorContext.log.info("Started gmail service")

        timeKeeper !! TimeKeeper.RemindMeEvery(pollingMinutes.minutes, context.self, HeartBeat, Some(this))

        implicit val requestGmailAsync: () => Unit = () => {
          implicit val ec: ExecutionContextExecutor = context.system.httpExecutionContext
          context.pipeToSelf(TinkerGmailService.fetchEmails(gmailService))(ReceiveInbox)
        }

        active(Set.empty)

      case Invalid(e) =>
        context.actorContext.log.warn(s"Failed to start ${noteRef.noteId}: $e")
        Tinker.ignore
    }
  }

  private def active(subscribers: Set[SpiritRef[Seq[Email]]])(implicit Tinker: Tinker, gmailService: Gmail, requestGmailAsync: () => Unit, noteRef: NoteRef, timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val tc: TinkerContext[_] = context

    message match {
      case Subscribe(subscriber) =>
        context.actorContext.log.info("New subscriber added: {}", subscriber)
        active(subscribers + subscriber)

      case ReceivePing(_) =>
        noteRef.getDocument() match {
          case Valid(Document(pollingInterval, GmailConfig(_, _), checkboxIsChecked)) =>
            if (checkboxIsChecked) {
              context.actorContext.log.info("Note check box was check, requesting email async now")
              requestGmailAsync()
              noteRef.setMarkdown("- [ ] Refresh now\n")
              // reset the timer
              timeKeeper !! TimeKeeper.RemindMeEvery(pollingInterval.minutes, context.self, HeartBeat, Some(this))
            } else {
              context.actorContext.log.debug("ignoring note ping")
            }

          case Invalid(e) =>
            context.actorContext.log.error(s"Something(s) went wrong reading ${noteRef.noteId}: $e")
        }

        Tinker.steadily

      case HeartBeat =>
        context.actorContext.log.info(s"Heart beat, fetching emails async")
        requestGmailAsync()
        Behaviors.same

      case ReceiveInbox(maybeEmails) =>
        maybeEmails match {
          case Failure(exception) => context.actorContext.log.error("Failed to fetch Gmail", exception)
          case Success(newEmails) =>
            context.actorContext.log.info(s"Fetched ${newEmails.size} emails; sending to subscribers $subscribers")
            subscribers.foreach(_ !! newEmails.sortBy(_.sentAt))
        }

        Behaviors.same
    }
  }

  //

  private[google] case class Document(pollingMinutes: Int, gmailConfig: GmailConfig, checkboxIsChecked: Boolean)

  private[google] implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def getDocument(): ValidatedNel[String, Document] = {
      noteRef.read() match {
        case Failure(exception) => throw exception
        case Success(note@Note(markdown, _)) =>
          note.yamlFrontMatter match {
            case Failure(exception: org.yaml.snakeyaml.scanner.ScannerException) =>
              s"YAML parsing failure ${Common.getStackTraceString(exception)}".invalidNel
            case Failure(exception) =>
              throw exception
            case Success(map) =>
              val validatedPollingMinutes = map.get("polling_minutes") match {
                case Some(value: Int) =>
                  value.validNel
                case other => s"Expected a string for key polling_minutes but found: $other".invalidNel
              }

              val validatedTokenPath = getValidatedStringFromConfig(map, "token_path")
              val validatedCredsPath = getValidatedStringFromConfig(map, "creds_path")

              validatedPollingMinutes.andThen { pollingMinutes =>
                validatedTokenPath.andThen { tokenPath =>
                  validatedCredsPath.andThen { credsPath =>
                    Document(
                      pollingMinutes,
                      GmailConfig(credsPath, tokenPath),
                      markdown.startsWith("- [x] ")
                    ).validNel
                  }
                }
              }
          }
      }
    }
  }
}

case class GmailConfig(credentialsPath: String, tokensPath: String)

private object TinkerGmailService {
  def createGmailService(config: GmailConfig): Gmail = {
    val credential = GmailAuth.authenticate(config.credentialsPath, config.tokensPath)
    new Gmail.Builder(GoogleNetHttpTransport.newTrustedTransport(), GsonFactory.getDefaultInstance, credential)
      .setApplicationName("Gmail Actor Service") // FIXME: ApplicationName
      .build()
  }

  def fetchEmails(gmailService: Gmail)(implicit executionContextExecutor: ExecutionContextExecutor): Future[Seq[Email]] = (Future {
    val messages: List[com.google.api.services.gmail.model.Message] = {
      val rawMessages = gmailService.users().messages().list("me")
        // FIXME
        .setLabelIds(List("INBOX").asJava)
        .setQ("is:unread")
        .setMaxResults(5).execute().getMessages

      Option(rawMessages).map(_.asScala.toList).getOrElse(Nil)
    }

    messages.flatMap { msg =>
      // FIXME: should these be Futures?
      val fullMessage = gmailService.users().messages().get("me", msg.getId).execute()
      val payload: MessagePart = fullMessage.getPayload
      val headers = payload.getHeaders.asScala

      // FIXME: https://github.com/micseydel/tinker-casting/issues/21 verify sender
      val sender = headers.find(_.getName == "From").map(_.getValue).getOrElse("Unknown")
      val subject = headers.find(_.getName == "Subject").map(_.getValue).getOrElse("No Subject")

      val parts = payload.getParts
      val body = if (parts != null) extractPlainText(parts)
      else Option(payload.getBody).map(body => {
        Option(body.decodeData()).map(_.map(_.toChar).mkString).getOrElse("")
      }).getOrElse("(No Content)")

      val time = getSentTimeFromHeaders(payload)

      val groupedHeaders = headers.groupBy(_.getName).map { case (k, v) => k -> v.map(_.getValue).toList }

      Some(Email(sender, subject, body, time, groupedHeaders))
    }
  }).recoverWith {
    case e: com.google.api.client.auth.oauth2.TokenResponseException =>
      if (e.getDetails.getError == "invalid_grant" && e.getDetails.getErrorDescription == "Token has been expired or revoked") {
        Future.failed(new RuntimeException(s"This happens once a week, delete the file: .gmail_tokens.json/StoredCredential"))
      } else {
        Future.failed(e)
      }
  }

  private def extractPlainText(parts: java.util.List[MessagePart]): String = {
    parts.asScala.collectFirst {
      case part if part.getMimeType == "text/plain" =>
        new String(Base64.getUrlDecoder.decode(part.getBody.getData))
    }.getOrElse("[No plain text content found]")
  }

  private def getSentTimeFromHeaders(payload: MessagePart): String = {
    val headers = payload.getHeaders.asScala

    headers.find(_.getName.equalsIgnoreCase("Date"))
      .map { header =>
        val raw = header.getValue
        val cleanedDate = raw.replaceAll("\\s*\\(.*\\)", "") // Remove (PDT), (UTC), etc.
        Try(ZonedDateTime.parse(cleanedDate, DateTimeFormatter.RFC_1123_DATE_TIME)) match {
          case Failure(exception: DateTimeParseException) =>
            // FIXME: log!
            raw
          case Failure(exception) =>
            throw exception
          case Success(date) => date.toString
        }
      }
      .getOrElse(ZonedDateTime.now(ZoneId.systemDefault()))
      .toString
  }
}


object GmailAuth {
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
        CalendarScopes.CALENDAR_SETTINGS_READONLY
      ).asJava
    ).setDataStoreFactory(new FileDataStoreFactory(new File(tokensDir)))
      .setAccessType("offline")
      .build()

    // FIXME: use event logger instead?
    val receiver = new LocalServerReceiver.Builder().setPort(8888).build()

    new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
  }
}
