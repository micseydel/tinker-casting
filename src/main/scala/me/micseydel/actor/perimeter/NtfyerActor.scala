package me.micseydel.actor.perimeter

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.StreamTcpException
import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerContext}
import me.micseydel.vault.Note
import net.jcazevedo.moultingyaml.*

import java.io.FileNotFoundException
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object NtfyerActor {
  // mailbox

  sealed trait Message

  case class DoNotify(key: String, message: String) extends Message

  private case class ReceivePing(ping: Ping) extends Message

  // key -> response
  private case class ReceiveNetworkReply(response: (String, Try[HttpResponse])) extends Message

  private case class ReceiveUnmarshaling(payload: (String, StatusCode, Try[String])) extends Message

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, ReceivePing]("NtfyActor", rgb(0, 255, 255), "📧", ReceivePing) { (context, noteRef) =>
      implicit val s: ActorSystem[?] = context.system.actorSystem
      implicit val c: TinkerContext[Message] = context

      noteRef.readNote()
        .recoverWith {
          case _: FileNotFoundException =>
            Success(Note("", None))
        }
        .flatMap {
          case Note(markdown, maybeFrontmatter) =>
            if (markdown.isEmpty && maybeFrontmatter.isEmpty) {
              noteRef.setRaw(
                """---
                  |key: (fill in)
                  |message: (fill in)
                  |---
                  |- [ ] Send
                  |""".stripMargin)
            } else {
              Success(NoOp)
            }
        } match {
        case Failure(exception) => context.actorContext.log.warn(s"Failed to read note on startup", exception)
        case Success(NoOp) =>
      }

      Tinker.receiveMessage {
        case DoNotify(key, message) =>
          context.actorContext.log.info(s"Sending HTTP POST request to ntfy")
          makeRequest(key, message)
          Tinker.steadily

        case ReceiveNetworkReply((key, response)) =>
          response match {
            case Failure(exception: StreamTcpException) =>
              context.actorContext.log.error(s"NtfyHttpCallResult failed for key $key; is there an internet connection?", exception)
            case Failure(exception) =>
              context.actorContext.log.warn("ntfy call failed", exception)
            case Success(httpResponse: HttpResponse) =>
              if (httpResponse.status == StatusCode.int2StatusCode(200)) {
                httpResponse.discardEntityBytes()
                context.actorContext.log.debug(s"Success 200, discarding entity bytes")
              } else {
                context.actorContext.log.warn(s"Unexpected status code ${httpResponse.status.intValue()}, ${httpResponse.status.reason()}; unmarshaling the payload for details now...")
                context.pipeToSelf(Unmarshal(httpResponse.entity).to[String])(unmarshaling => ReceiveUnmarshaling((key, httpResponse.status, unmarshaling)))
              }
          }

          Tinker.steadily

        case ReceiveUnmarshaling((key, httpStatusCode, responseString)) =>
          responseString match {
            case Failure(exception) => context.actorContext.log.error(s"Got ${httpStatusCode.intValue()} on $key but failed to unmarshal payload (as a string!)", exception)
            case Success(reason) => context.actorContext.log.warn(s"Got ${httpStatusCode.intValue()} on $key: $reason")
          }

          Tinker.steadily

        case ReceivePing(NoOp) =>
          noteRef.readNote().flatMap { case Note(markdown, maybeFrontmatter) =>
            if (markdown.startsWith("- [x] ")) {
              maybeFrontmatter match {
                case None =>
                  context.actorContext.log.warn(s"Got a button press, but frontmatter is empty; not sending any push notification")
                  Success(NoOp)
                case Some(frontmatter) =>
                  import YamlProtocol.frontMatterYamlFormat
                  Try(frontmatter.parseYaml.convertTo[FrontMatter]).flatMap { case FrontMatter(key, message) =>
                    context.actorContext.log.info(s"Sending HTTP POST request to ntfy on key $key (triggered via note)")
                    makeRequest(key, message)
                    noteRef.setMarkdown("- [ ] Send")
                  }
              }
            } else {
              context.actorContext.log.debug("ignoring note ping")
              Success(NoOp)
            }
          } match {
            case Failure(exception) => context.actorContext.log.warn(s"Unexpected exception", exception)
            case Success(NoOp) =>
          }

          Tinker.steadily
      }
    }

  //

  private def makeRequest(key: String, message: String)(implicit context: TinkerContext[Message], actorSystem: ActorSystem[?]): Unit = {
    val uri = s"https://ntfy.sh/$key" // FIXME: allow for self-hosted ntfy
    context.pipeToSelf(request(uri, message))(networkReply => ReceiveNetworkReply(key -> networkReply))
  }

  private def request(uri: String, message: String)(implicit s: ActorSystem[?]): Future[HttpResponse] = {
    val payload = HttpEntity(ContentTypes.`application/x-www-form-urlencoded`, message)
    Http().singleRequest(HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = payload
    ))
  }

  //

  private case class FrontMatter(key: String, message: String)

  private object YamlProtocol extends DefaultYamlProtocol {
    implicit val frontMatterYamlFormat: YamlFormat[FrontMatter] = yamlFormat2(FrontMatter)
  }
}
