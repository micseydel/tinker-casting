package me.micseydel.actor.notifications

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse}
import me.micseydel.NoOp
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.notifications.ChimeActor.{BirSur, Chime, Command, Error, Info, Mario, Material, Pokemon, Sonic, Success, Theme, Warning, Zelda}
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext, TinkerContextImpl}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
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

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing]("Chime config", TinkerColor.random(), "🔔", ReceivePing) { (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context

    noteRef.setMarkdown("- [ ] Test")

    context.self !! ReceivePing(NoOp)
    initializing(noteRef)
  }

  private def initializing(noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case ReceivePing(_) =>
        noteRef.readNote().flatMap(_.yamlFrontMatter) match {
          case Failure(exception) => throw exception

          case util.Success(yaml: Map[String, Any]) =>
            yaml.get("host") match {
              case Some(host: String) =>
                behavior(host, noteRef)

              case other =>
                context.actorContext.log.warn(s"Expected a string host, found $other")
                initializing(noteRef)
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
        noteRef.readNote().map(_.markdown).map(_(3)) match {
          case Failure(exception) => context.actorContext.log.warn(s"Something went wrong checking the chime note", exception)
          case util.Success('x') =>
            noteRef.setMarkdown("- [ ] Test")
            context.self !! Success(Material)

          case util.Success(_) =>
            context.actorContext.log.warn("Ignored note ping")
        }
        Tinker.steadily
    }
  }

  //

  private def doAsyncHttp(host: String, function: String, theme: Theme)(implicit httpExecutionContext: ExecutionContext, system: ActorSystem[Nothing]): Unit = {
    val payload = Map(
      "function" -> function,
      "theme" -> theme.str
    )

    import spray.json.DefaultJsonProtocol._
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

    def unapply(string: String): Option[Theme] = Mapping.get(string)
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
