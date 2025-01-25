package me.micseydel.actor

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse, ResponseEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext, Tinkerer}
import me.micseydel.dsl.Tinker.Ability
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import me.micseydel.actor.VaultPathAdapter.VaultPathUpdatedEvent
import me.micseydel.vault.persistence.NoteRef

import scala.annotation.unused
import scala.concurrent.Future
import scala.util.{Failure, Success}

object WyzeActor {
  sealed trait Message

  private final case class ReceiveWyzeAPIResponse(response: WyzeAPIResponse) extends Message
  private final case class ReceiveVaultPathUpdatedEvent(vaultPathUpdatedEvent: VaultPathUpdatedEvent) extends Message

  private val NoteName = "Wyze tinkering"

  def apply(wyzeUri: String)(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor.rgb(0, 255, 255), "🔌").setup { _ =>
    Tinker.initializedWithNote(NoteName, "_actor_notes/wyze") { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context

      val request = HttpRequest(
        method = HttpMethods.GET,
        uri = s"http://$wyzeUri/wyze/plug"
      )

      context.system.actorNotesFolderWatcherActor !! ActorNotesFolderWatcherActor.Subscribe("wyze", context.messageAdapter(ReceiveVaultPathUpdatedEvent))

      import WyzeAPIJsonProtocol.wyzeAPIResponseResultJsonFormat

      @unused // fetches and then dies
      val devicesFetcher: SpiritRef[HttpFetchAndUnmarshall.Message[WyzeAPIResponse]] =
        context.castAnonymous(HttpFetchAndUnmarshall(request, context.messageAdapter(ReceiveWyzeAPIResponse), FailedWyzeAPIResponse))

      initializing(noteRef)
    }
  }

  private def initializing(noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveVaultPathUpdatedEvent(event) =>
        context.actorContext.log.info(s"Not yet initialized but received event $event")
        Tinker.steadily

      case ReceiveWyzeAPIResponse(response) =>
        response match {
          case FailedWyzeAPIResponse(exception) =>
            context.actorContext.log.error("Failed to get device list", exception)
            Tinker.steadily
          case WyzeAPIResponseResult(wyzeDeviceList) =>
            // FIXME: cache the is_on state, have an initiaization
            context.actorContext.log.info(s"Received device list, writing to $NoteName a total of ${wyzeDeviceList.size} devices")

            noteRef.setMarkdown {
              val devices = wyzeDeviceList.map {
                case OutdoorPlug(ip, mac, nickname, maybe_is_on) =>
                  maybe_is_on match {
                    case Some(true) =>
                      s"- [ ] $nickname: $mac"
                    case Some(false) =>
                      s"- [x] $nickname: $mac"
                    case None =>
                      s"- $nickname: $mac"
                  }
              }.mkString("\n") + "\n"

              s"""- generated ${context.system.clock.now()}
                 |# Devices
                 |
                 |$devices""".stripMargin
            }

            val isOnMap = wyzeDeviceList.flatMap {
              case OutdoorPlug(ip, mac, nickname, maybe_is_on) =>
                maybe_is_on.map(mac -> _)
            }.toMap

            initialized(isOnMap)(Tinker, noteRef)
        }
    }
  }

  private val Off = 'x'
  private val On = ' '

  private def initialized(isOnMap: Map[String, Boolean])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveVaultPathUpdatedEvent(event) =>
        event match {
          case VaultPathAdapter.PathModifiedEvent(_) =>
            context.actorContext.log.info("File updated, reading Markdown...")
            noteRef.readMarkdown() match {
              case Failure(exception) => throw exception
              case Success(markdown) =>
                val isOnMapFromDisk = markdown.split("\n")
                  .filter(_.startsWith("- ["))
                  .flatMap { line =>
                    val maybeOnOff: Option[Boolean] = line(3) match {
                      case Off => Some(false)
                      case On => Some(true)
                      case other =>
                        context.actorContext.log.warn(s"Wierd line: $line")
                        None
                    }

                    maybeOnOff.flatMap { onOff =>
                      line.split(": ").toList match {
                        case List(_, mac) =>
                          Some(mac -> onOff)
                        case _ =>
                          context.actorContext.log.warn(s"Expected nickname:mac but got `$line``")
                          None
                      }
                    }
                  }.toMap

                isOnMap.foreach { case (mac, cachedState) =>
                  isOnMapFromDisk.get(mac) match {
                    case Some(stateOnDisk) =>
                      if (stateOnDisk != cachedState) {
                        context.actorContext.log.info(s"FYI, mac $mac changed its on/off state from $cachedState to $stateOnDisk")
                      }

                    case None =>
                  }
                }
            }

          case VaultPathAdapter.PathCreatedEvent(_) | VaultPathAdapter.PathDeletedEvent(_) => ???
        }
        Tinker.steadily

      case ReceiveWyzeAPIResponse(response) =>
        context.actorContext.log.warn(s"Did not expect a Wyze response after initialization: $response")
        Tinker.steadily
    }
  }

  // model

  sealed trait WyzeAPIResponse

  case class FailedWyzeAPIResponse(exception: Throwable) extends WyzeAPIResponse

  case class WyzeAPIResponseResult(wyze_plug_list: List[WyzeDevice]) extends WyzeAPIResponse

  // api

  sealed trait WyzeDevice

  case class OutdoorPlug(ip: String, mac: String, nickname: String, is_on: Option[Boolean]) extends WyzeDevice

  object WyzeAPIJsonProtocol extends DefaultJsonProtocol {
    implicit val outdoorPlugJsonFormat: RootJsonFormat[OutdoorPlug] = jsonFormat4(OutdoorPlug)

    implicit object WyzeDeviceJsonFormat extends RootJsonFormat[WyzeDevice] {
      def write(m: WyzeDevice): JsValue = {
        m match {
          case op: OutdoorPlug =>
            JsObject(op.toJson.asJsObject.fields + ("type" -> JsString("OutdoorPlug")))
        }
      }

      def read(value: JsValue): WyzeDevice = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("OutdoorPlug" | "Plug")) => value.convertTo[OutdoorPlug]
          case other => throw DeserializationException(s"""Unknown type $other; expected Seq(JsString("OutdoorPlug"))""")
        }
      }
    }

    implicit val wyzeAPIResponseResultJsonFormat: RootJsonFormat[WyzeAPIResponseResult] = jsonFormat1(WyzeAPIResponseResult)
  }
}


object HttpFetchAndUnmarshall {
  sealed trait Message[T]

  private case class ReceiveHttpResponse[T](httpResponse: HttpResponse) extends Message[T]

  private case class ReceiveFailedHttpResponse[T](exception: Throwable) extends Message[T]

  private case class ReceiveUnmarshalling[T](reply: T) extends Message[T]

  // behavior

  def apply[T, E <: T, R <: T](httpRequest: HttpRequest, replyTo: SpiritRef[T], failureWrapper: Throwable => E)(implicit Tinker: Tinker, jsonFormat: RootJsonFormat[R]): Ability[Message[T]] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    implicit val s: ActorSystem[_] = context.system.actorSystem

    context.pipeToSelf(Http().singleRequest(httpRequest)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }

    Tinker.withMessages {
      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received HttpResponse, beginning unmarshalling process")

        val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
        val fut: Future[String] = umarshal.to[String]

        context.pipeToSelf(fut) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(models) =>
            try {
              ReceiveUnmarshalling(models.parseJson.convertTo[R])
            } catch {
              case e: DeserializationException =>
                // FIXME: this capture should be opt-in via config due to risk of leaking sensitive data
                ReceiveFailedHttpResponse(new RuntimeException(s"failed to parse: $models", e))
            }
        }

        context.pipeToSelf(Unmarshal(httpResponse.entity).to[R]) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(chatResponse) => ReceiveUnmarshalling(chatResponse)
        }
        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.error("Something went wrong with HttpResponse", exception)
        replyTo !! failureWrapper(exception)
        Tinker.steadily

      case ReceiveUnmarshalling(response) =>
        context.actorContext.log.info("Unmarshalling succeeding, replying now")
        replyTo !! response
        Tinker.steadily // FIXME
    }
  }
}
