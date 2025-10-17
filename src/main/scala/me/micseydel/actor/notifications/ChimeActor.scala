package me.micseydel.actor.notifications

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.notifications.ChimeActor.{Command, Error, Info, Success, Theme, Warning}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.{Failure, Try}

object ChimeActor {
  sealed trait Message

  private case class ReceivePing(ping: Ping) extends Message

  sealed trait Command extends Message {
    val theme: Theme
  }

  final case class Success(theme: Theme) extends Command

  final case class Warning(theme: Theme) extends Command

  final case class Info(theme: Theme) extends Command

  final case class Error(theme: Theme) extends Command

  // model

  sealed abstract class Theme(val str: String)

  case object BirSur extends Theme("big-sur")

  case object Chime extends Theme("chime")

  case object Mario extends Theme("mario")

  case object Material extends Theme("material")

  case object Pokemon extends Theme("pokemon")

  case object Sonic extends Theme("sonic")

  case object Zelda extends Theme("zelda")

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("Chime config", TinkerColor.random(), "🔔", ReceivePing, Some("_actor_notes")) { (context, noteRef) =>
    initializing(noteRef)
  }

  private def initializing(noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>

    noteRef.resetMarkdown()
    context.self !!!! ReceivePing(NoOp) // the above doesn't trigger a ping 100% of the time 🤷

    Tinker.receiveMessage {
      case ReceivePing(_) =>
        noteRef.readNote().flatMap(_.yamlFrontMatter) match {
          case Failure(exception) =>
            context.actorContext.log.warn(s"Broken frontmatter for ${noteRef.noteId}, ignoring", exception)
            Tinker.steadily

          case util.Success(yaml: Map[String, Any]) =>
            yaml.get("host") match {
              case Some(host: String) =>
                behavior(host, noteRef)

              case other =>
                context.actorContext.log.warn(s"Expected a string host, found $other (ignoring)")
                Tinker.steadily
            }
        }

      case command: Command =>
        context.actorContext.log.warn(s"Ignoring command $command, not yet configured")
        Tinker.steadily
    }
  }

  private def behavior(host: String, noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context
    implicit val actorSystem: ActorSystem[_] = context.system.actorSystem
    implicit val ec: ExecutionContextExecutorService = context.system.httpExecutionContext

    Tinker.receiveMessage {
      case Success(theme) =>
        context.actorContext.log.info(s"Calling chime.success with theme $theme")
        doAsyncHttp(host, "success", theme)
        Tinker.steadily

      case Warning(theme) =>
        context.actorContext.log.info(s"Calling chime.warning with theme $theme")
        doAsyncHttp(host, "warning", theme)
        Tinker.steadily

      case Info(theme) =>
        context.actorContext.log.info(s"Calling chime.info with theme $theme")
        doAsyncHttp(host, "info", theme)
        Tinker.steadily

      case Error(theme) =>
        context.actorContext.log.info(s"Calling chime.error with theme $theme")
        doAsyncHttp(host, "error", theme)
        Tinker.steadily

      case ReceivePing(_) =>
        context.actorContext.log.debug("Got a note ping...")
        noteRef.readNote().flatMap { note =>
          val maybeUpdatedTheme = getFirstSelectedListItemForHeader(note.markdown, "Select a Theme")
          val maybeSelectedCommand = getFirstSelectedListItemForHeader(note.markdown, "Issue Command")

          val themeFromDisk: Try[Theme] = maybeUpdatedTheme.flatMap(Theme.unapply) match {
            case Some(selectedTheme) =>
              // if the theme was updated via Markdown, we cache it in the yaml
              noteRef.setTo(Note(DefaultMarkdown, Map("host" -> host, "theme" -> selectedTheme.str)))
                .map(_ => selectedTheme)

            case None =>
              // if it wasn't updated, we try to fetch it from the cache and use a default
              note.yamlFrontMatter.map(_.get("theme")).map(_.collect {
                case Theme(theme) => theme
              }.getOrElse(Material))
          }

          themeFromDisk.map { theme =>
            val maybeCommand = maybeSelectedCommand match {
              case Some("Success") => Some(Success(theme))
              case Some("Warning") => Some(Warning(theme))
              case Some("Info") => Some(Info(theme))
              case Some("Error") => Some(Error(theme))

              case Some(other) =>
                context.actorContext.log.warn(s"Ignoring unrecognized command: $other")
                None

              case None => None
            }

            for (command <- maybeCommand) {
              context.self !! command
              // if we issue a command but the theme wasn't updated, that means we need to
              // reset the markdown because it didn't happen above
              if (maybeUpdatedTheme.isEmpty) {
                noteRef.resetMarkdown()
              }
            }
          }
        } match {
          case Failure(exception) => throw exception
          case util.Success(_) =>
        }

        Tinker.steadily
    }
  }

  private def getFirstSelectedListItemForHeader(markdown: String, header: String): Option[String] = {
    markdown.split("\n")
      .dropWhile(_ != s"# $header")
      .drop(1)
      .dropWhile(!_.startsWith("- ["))
      .takeWhile(_.startsWith("- ["))
      .collectFirst {
        case s if s.length > 3 && s.charAt(3) == 'x' =>
          s.drop(6)
      }
  }

  //

  private def doAsyncHttp(host: String, function: String, theme: Theme)(implicit httpExecutionContext: ExecutionContext, system: ActorSystem[Nothing]): Unit = {
    val payload = Map(
      "function" -> function,
      "theme" -> theme.str
    )

    import spray.json.DefaultJsonProtocol.*
    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(
        method = HttpMethods.POST,
        uri = s"http://$host/chime",
        entity = HttpEntity(ContentTypes.`application/json`, payload.toJson.toString)
      )
    )

    responseFuture.onComplete {
      case scala.util.Success(res) =>
        //        println(res)
        res.discardEntityBytes()
      case Failure(t) =>
        system.log.warn(s"HTTP call failed for $host", t)
    }
  }

  object Theme {
    private val Mapping = Map(
      "big-sur" -> BirSur,
      "chime" -> Chime,
      "mario" -> Mario,
      "material" -> Material,
      "pokemon" -> Pokemon,
      "sonic" -> Sonic,
      "zelda" -> Zelda
    )

    val Themes: Set[String] = Mapping.keySet
    val ThemesList: List[String] = Theme.Themes.toList.sorted

    def unapply(string: String): Option[Theme] = Mapping.get(string)
  }

  private val DefaultMarkdown = {
    val themesMarkdownList = Theme.ThemesList.mkString("- [ ] ", "\n- [ ] ", "\n")

    s"""# Issue Command
       |
       |- [ ] Success
       |- [ ] Warning
       |- [ ] Info
       |- [ ] Error
       |
       |# Select a Theme
       |
       |$themesMarkdownList"""
  }

  private implicit class RichChimeNoteRef(val noteRef: NoteRef) extends AnyVal {
    def resetMarkdown(): Unit = {
      noteRef.setMarkdown(DefaultMarkdown) match {
        case Failure(exception) => throw exception
        case util.Success(NoOp) =>
      }
    }
  }
}

object ChimeJsonFormat extends DefaultJsonProtocol {
  implicit object ThemeJsonFormat extends RootJsonFormat[Theme] {
    def write(m: Theme): JsValue = {
      JsString(m.str)
    }

    def read(value: JsValue): Theme = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString(Theme(theme))) => theme
        case other => throw DeserializationException(s"Unknown type, expected Seq(JsString(THEME)) with THEME in ${Theme.Themes} but got $other")
      }
    }
  }

  implicit val successJsonFormat: JsonFormat[Success] = jsonFormat1(Success)
  implicit val warningJsonFormat: JsonFormat[Warning] = jsonFormat1(Warning)
  implicit val infoJsonFormat: JsonFormat[Info] = jsonFormat1(Info)
  implicit val errorJsonFormat: JsonFormat[Error] = jsonFormat1(Error)

  implicit object ChimeMessageJsonFormat extends RootJsonFormat[Command] {
    def write(m: Command): JsValue = {
      val (jsObj, typ) = m match {
        case l: Success => (l.toJson.asJsObject, "Success")
        case l: Warning => (l.toJson.asJsObject, "Warning")
        case l: Info => (l.toJson.asJsObject, "Info")
        case l: Error => (l.toJson.asJsObject, "Error")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): Command = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("Success")) => value.convertTo[Success]
        case Seq(JsString("Warning")) => value.convertTo[Warning]
        case Seq(JsString("Info")) => value.convertTo[Info]
        case Seq(JsString("Error")) => value.convertTo[Error]
        case other => throw DeserializationException(s"Unknown type, expected Seq(JsString(_)) for one of {Success, Warning, Error, Info} but got $other")
      }
    }
  }
}
