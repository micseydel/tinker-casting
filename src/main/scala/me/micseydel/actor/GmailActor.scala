package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import com.google.api.client.auth.oauth2.{AuthorizationCodeFlow, Credential}
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.gmail.Gmail
import com.google.api.services.gmail.model.{MessagePart, MessagePartHeader}
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.GmailActor.Email
import me.micseydel.dsl.{Operator, SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef

import java.io.{File, FileInputStream, InputStreamReader}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, DateTimeParseException}
import java.time.{ZoneId, ZonedDateTime}
import java.util.{Base64, Collections, Locale}
import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
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

  def apply(config: GmailConfig)(implicit Tinker: Tinker): Ability[Message] = {
    AttentiveNoteMakingTinkerer[Message, ReceivePing]("Gmail Configuration", TinkerColor.random(), "💌", ReceivePing) { case (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context
      implicit val nr: NoteRef = noteRef
      implicit val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

      context.system.operator !! Operator.RegisterGmail(context.self)

      context.actorContext.log.info("Starting gmail service")
      implicit val gmailService: Gmail = TinkerGmailService.createGmailService(config)
      context.actorContext.log.info("Started gmail service")

      val pollingMinutes = noteRef.getConfigOrDefault()
      timeKeeper !! TimeKeeper.RemindMeEvery(pollingMinutes, context.self, HeartBeat, Some(this))

      implicit val requestGmailAsync: () => Unit = () => {
        implicit val ec: ExecutionContextExecutor = context.system.httpExecutionContext
        context.pipeToSelf(TinkerGmailService.fetchEmails(gmailService))(ReceiveInbox)
      }

      active(Set.empty)
    }
  }

  private def active(subscribers: Set[SpiritRef[Seq[Email]]])(implicit Tinker: Tinker, gmailService: Gmail, requestGmailAsync: () => Unit, noteRef: NoteRef, timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val tc: TinkerContext[_] = context

    message match {
      case Subscribe(subscriber) =>
        context.actorContext.log.info("New subscriber added: {}", subscriber)
        active(subscribers + subscriber)

      case ReceivePing(_) =>
        if (noteRef.checkBoxIsChecked()) {
          context.actorContext.log.info("Note check box was check, requesting email async now")
          requestGmailAsync()
          // FIXME: only need to read from the NoteRef once, rather than for the checkbox above then config below
          noteRef.setMarkdown("- [ ] Refresh now\n")
          // reset the timer
          timeKeeper !! TimeKeeper.RemindMeEvery(noteRef.getConfigOrDefault(), context.self, HeartBeat, Some(this))
        } else {
          context.actorContext.log.debug("ignoring note ping")
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

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown()
        .map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }

    def getConfigOrDefault(default: Int = 5): FiniteDuration = {
      (noteRef.read()
        .flatMap(_.yamlFrontMatter)
        .map(_.get("polling_minutes")) match {
        case Success(Some(value: Int)) => value
        case _ => default
      }).minutes
    }
  }
}

case class GmailConfig(credentialsPath: String, tokensPath: String)

private object TinkerGmailService {
  def createGmailService(config: GmailConfig): Gmail = {
    val credential = GmailAuth.authenticate(config.credentialsPath, config.tokensPath)
    new Gmail.Builder(GoogleNetHttpTransport.newTrustedTransport(), GsonFactory.getDefaultInstance, credential)
      .setApplicationName("Gmail Actor Service")
      .build()
  }

  def fetchEmails(gmailService: Gmail)(implicit executionContextExecutor: ExecutionContextExecutor): Future[Seq[Email]] = Future {
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
      Collections.singletonList("https://www.googleapis.com/auth/gmail.readonly")
    ).setDataStoreFactory(new FileDataStoreFactory(new File(tokensDir)))
      .setAccessType("offline")
      .build()

    // FIXME: use event logger instead?
    val receiver = new LocalServerReceiver.Builder().setPort(8888).build()

    new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
  }
}
