package me.micseydel.app

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, ResponseEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.rometools.rome.feed.synd.{SyndContent, SyndFeed}
import com.rometools.rome.io.SyndFeedInput
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}

import java.io.StringReader
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.{Failure, Success, Try}


object RomeRssTestingActor {
  sealed trait Message

  private case class ReceiveHttpResult(httpResponse: Try[HttpResponse]) extends Message

  private case class ReceiveUnmarshalling(raw: Try[String]) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("RomeRssTestingActor", TinkerColor.random(), "🙉") { (context, noteRef) =>
    implicit val s: ActorSystem[?] = context.system.actorSystem

    // "https://stackoverflow.com/feeds/tag?tagnames=rome"
    val uri = "https://www.reddit.com/r/openclaw.rss"
    context.pipeToSelf(Http().singleRequest(
      HttpRequest(method = HttpMethods.GET, uri = uri)
    ))(ReceiveHttpResult)

    Tinker.receiveMessage {
      case ReceiveHttpResult(httpResult) =>
        httpResult match {
          case Failure(exception) => throw exception
          case Success(httpResponse) =>
            val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
            context.pipeToSelf(umarshal.to[String])(ReceiveUnmarshalling)
        }
        Tinker.steadily

      case ReceiveUnmarshalling(rawResult) =>
        rawResult.flatMap(rawWithBuilt) match {
          case Failure(exception) => throw exception
          case Success((raw, feed)) =>

            val formattedEntries: String = feed.getEntries.asScala.zipWithIndex.map { case (entry, i) =>
              val formattedContents: String = {
                val contents: mutable.Seq[SyndContent] = entry.getContents.asScala

                if (contents.isEmpty) {
                  "empty? 🤷"
                } else if (contents.size == 1) {
                  s"""
                     |```html
                     |${contents.head.getValue}
                     |```""".stripMargin
                } else {
                  contents.zipWithIndex.map { case (contentsj, j) =>
                    s"""### contents ($i, $j)
                       |
                       |```html
                       |$contentsj
                       |```
                       |""".stripMargin
                  }.mkString("\n")
                }
              }

              s"""## ${entry.getAuthor} @ ${entry.getPublishedDate} ($i)
                 |
                 |- **${entry.getTitle}**
                 |$formattedContents
                 |
                 |### Raw entry ($i)
                 |
                 |```
                 |$entry
                 |```
                 |""".stripMargin
            }.mkString("\n")

            noteRef.setMarkdown(
              s"""- generated ${context.system.clock.now()}
                 |- ${feed.getEntries.size()} entries
                 |
                 |# Entries
                 |
                 |$formattedEntries
                 |
                 |# Full Feed
                 |
                 |```
                 |$feed
                 |```
                 |
                 |# Raw
                 |
                 |```xml
                 |$raw
                 |```
                 |""".stripMargin)
        }

        Tinker.steadily
    }
  }

  private def rawWithBuilt(raw: String): Try[(String, SyndFeed)] = Try {
    val input: SyndFeedInput = new SyndFeedInput()
    val feed: SyndFeed = input.build(new StringReader(raw))
    (raw, feed)
  }
}
