package me.micseydel.app

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, ResponseEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import cats.data.ValidatedNel
import com.rometools.rome.feed.synd.{SyndContent, SyndFeed}
import com.rometools.rome.io.SyndFeedInput
import me.micseydel.{Common, NoOp}
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import java.io.StringReader
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.{Failure, Success, Try}


object RomeRssTestingActor {
  sealed trait Message

  private case class ReceiveNotePing(ping: Ping) extends Message

  private case class ReceiveHttpResult(httpResponse: Try[HttpResponse]) extends Message

  private case class ReceiveUnmarshalling(raw: Try[String]) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("RomeRssTestingActor", TinkerColor.random(), "🙉", ReceiveNotePing) { (context, noteRef) =>
    implicit val s: ActorSystem[?] = context.system.actorSystem

    // "https://stackoverflow.com/feeds/tag?tagnames=rome"
    //    val uri = "https://www.reddit.com/r/openclaw.rss"
//    val uri = "https://feeds.simplecast.com/RqijYekb"
    noteRef.setMarkdown(s"""- [ ] *fetch now*""") match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    Tinker.receiveMessage {
      case ReceiveNotePing(NoOp) =>
        noteRef.checkBoxAndMaybeUri() match {
          case Failure(exception) => context.actorContext.log.warn("Checking note failed", exception)
          case Success((true, Some(uri))) =>
            context.actorContext.log.info("Fetching feed...")
            context.pipeToSelf(Http().singleRequest(
              HttpRequest(method = HttpMethods.GET, uri = uri)
            ))(ReceiveHttpResult)
          case Success((boxChecked, maybeUri)) =>
            context.actorContext.log.debug(s"(boxChecked, maybeUri) = ($boxChecked, $maybeUri)")
        }

        Tinker.steadily

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

//            val t = feed.getEntries.asScala
//              .map(entry =>
//                (entry.getTitle, entry.getEnclosures.asScala.map(_.getUrl).toList, entry.getPublishedDate)
//              )
//            val x = t.sortBy(_._3).map {
//              case y@(title, List(url), publishedDate) =>
//                val date = TimeUtil.pythonEpocheToZonedDateTime(publishedDate.getTime/1000).format(DateTimeFormatter.ISO_LOCAL_DATE)
//                val cleanTitle = title.replace("’", "").replace("?", "").replace(":", "-") // FIXME: quick and dirty
//                s"- curl -o '$cleanTitle ($date).mp3' '$url'"
//
//              case y@(title, url, publishedDate) =>
//                s"- ($publishedDate) $title ??- $url"
//            }.mkString("\n")

            noteRef.setMarkdown(
              s"""- [ ] *fetch now*
                 |- generated ${context.system.clock.now()}
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

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxAndMaybeUri(): Try[(Boolean, Option[String])] = {
      noteRef.readNote().flatMap { note =>
        val checkBoxIsChecked: Try[Boolean] = if (note.markdown.startsWith("- [")) {
          Try(note.markdown.charAt(3) == 'x')
        } else {
          Failure(new RuntimeException(s"Expected markdown to start with \"- [\" but instead it started with: ${note.markdown.take(4)}"))
        }

        val maybeConfig: Try[Option[String]] = {
          val maybeUri: Try[Option[Any]] = note.yamlFrontMatter.map(_.get("uri"))

          maybeUri.map {
            case Some(value: String) => Some(value)
            case None => None
            case Some(_) => None // FIXME better non-happy path
          }
        }

        checkBoxIsChecked
          .flatMap(checkBoxChecked => maybeConfig.map(checkBoxChecked -> _))
      }
    }
  }
}
