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

import java.io.{File, FileInputStream, InputStreamReader}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{ZoneId, ZonedDateTime}
import java.util.{Base64, Collections}
import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

object GmailActor {
  sealed trait Message

  case class Subscribe(subscriber: ActorRef[Seq[Email]]) extends Message

  private case object CheckInbox extends Message
  private case class ReceiveInbox(emails: Try[Seq[Email]]) extends Message

  //

  case class Email(sender: String, subject: String, body: String, sentAt: String, groupedHeaders: Map[String, List[String]])

  def apply(config: GmailConfig): Behavior[Message] = Behaviors.setup { context =>
    context.log.info("Starting gmail service")
    val gmailService = createGmailService(config)
    context.log.info("Started gmail service")
    context.system.scheduler.scheduleAtFixedRate(0.minute, 5.minutes)(() =>
      context.self ! CheckInbox
    )(context.executionContext)

    active(gmailService, Set.empty)
  }

  private def active(gmailService: Gmail, subscribers: Set[ActorRef[Seq[Email]]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    message match {
      case Subscribe(subscriber) =>
        context.log.info("New subscriber added: {}", subscriber)
        active(gmailService, subscribers + subscriber)

      case CheckInbox =>
        context.log.info(s"Fetching emails (async!)")
        implicit val ec: ExecutionContextExecutor = context.system.executionContext
        context.pipeToSelf(fetchEmails(gmailService))(ReceiveInbox)
        Behaviors.same

      case ReceiveInbox(maybeEmails) =>
        maybeEmails match {
          case Failure(exception) => context.log.error("Failed to fetch Gmail", exception)
          case Success(newEmails) =>
            context.log.info(s"Fetched ${newEmails.size} emails; sending to subscribers $subscribers")
            subscribers.foreach(_ ! newEmails.sortBy(_.sentAt))
        }

        Behaviors.same
    }
  }

  private def createGmailService(config: GmailConfig): Gmail = {
    val credential = GmailAuth.authenticate(config.credentialsPath, config.tokensPath)
    new Gmail.Builder(GoogleNetHttpTransport.newTrustedTransport(), GsonFactory.getDefaultInstance, credential)
      .setApplicationName("Gmail Actor Service")
      .build()
  }

  private def fetchEmails(gmailService: Gmail)(implicit executionContextExecutor: ExecutionContextExecutor): Future[Seq[Email]] = Future {
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

case class GmailConfig(credentialsPath: String, tokensPath: String)



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
