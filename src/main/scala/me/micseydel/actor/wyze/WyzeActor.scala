package me.micseydel.actor.wyze

import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.wyze.WyzePlugModel.{WyzePlug, WyzePlugAPIResponse, WyzePlugAPIResult}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

object WyzeActor {
  sealed trait Message

  final case class SetPlug(mac: String, onOffState: Boolean) extends Message

  private final case class ReceiveDeviceList(result: WyzePlugAPIResponse) extends Message

  private case class ReceiveNoteUpdatedPing(ping: Ping) extends Message

  private val NoteName = "Wyze Plugs"

  def apply(wyzeUri: String)(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNoteUpdatedPing](NoteName, TinkerColor.rgb(0, 255, 255), "🔌", ReceiveNoteUpdatedPing) { (context, noteRef) =>
    implicit val c: TinkerContext[_] = context

    val api = context.cast(WyzeAPIActor(wyzeUri), "WyzeAPIActor")
    api !! WyzeAPIActor.GetDevices(context.messageAdapter(ReceiveDeviceList))

    initializing(noteRef, api)
  }

  private def initializing(noteRef: NoteRef, api: SpiritRef[WyzeAPIActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case ReceiveNoteUpdatedPing(_) =>
        Tinker.steadily

      case SetPlug(mac, onOffState) =>
        api !! WyzeAPIActor.SetPlugIsOn(mac, onOffState)
        Tinker.steadily

      case ReceiveDeviceList(wyzePlugAPIResponse) =>
        val plugs = wyzePlugAPIResponse.getPlugsOrThrow
        noteRef.updateMarkdown(context.system.clock.now(), plugs)
        initialized(genIsOnMap(plugs))(Tinker, noteRef, api)
    }
  }

  private val Off = 'x'
  private val On = ' '

  private def initialized(isOnMap: Map[String, Boolean])(implicit Tinker: Tinker, noteRef: NoteRef, api: SpiritRef[WyzeAPIActor.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case ReceiveNoteUpdatedPing(_) =>
        context.actorContext.log.info("File updated, reading Markdown...")

        val markdown = noteRef.readMarkdown() match {
          case Failure(exception) => throw exception
          case Success(md) => md
        }

        if (markdown.contains("[ ] Refresh now")) {
          api !! WyzeAPIActor.GetDevices(context.messageAdapter(ReceiveDeviceList))
          Tinker.steadily
        } else {
          val isOnMapFromDisk = markdown.split("\n")
            .filter(_.startsWith("- ["))
            .flatMap { line =>
              val maybeOnOff: Option[Boolean] = line(3) match {
                case Off => Some(false)
                case On => Some(true)
                case '[' => None
                case _ =>
                  context.actorContext.log.warn(s"Weird line: $line")
                  None
              }

              maybeOnOff.flatMap { onOff =>
                line.drop(18).split("\\|").toList match {
                  case List(mac, _) =>
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
                  context.actorContext.log.info(s"MAC $mac changed its on/off state $cachedState->$stateOnDisk, updating")
                  api !! WyzeAPIActor.SetPlugIsOn(mac, stateOnDisk)
                }

              case None =>
            }
          }

          initialized(isOnMapFromDisk)
        }

      case SetPlug(mac, onOffState) =>
        api !! WyzeAPIActor.SetPlugIsOn(mac, onOffState)
        Tinker.steadily

      case ReceiveDeviceList(wyzePlugAPIResponse) =>
        val plugs = wyzePlugAPIResponse.getPlugsOrThrow
        noteRef.updateMarkdown(context.system.clock.now(), plugs)
        initialized(genIsOnMap(plugs))(Tinker, noteRef, api)
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def updateMarkdown(now: ZonedDateTime, plugs: List[WyzePlug]): Unit = {
      noteRef.setMarkdown {
        s"- From ${now.toString.slice(0, 16)}\n    - [ ] Refresh now\n" +
          plugs.sortBy {
            case WyzePlug(_, _, nickname, is_on) =>
              is_on match {
                case _ if nickname == "Blue Air" | nickname == "CR Box" => -3
                case Some(true) => -2
                case Some(false) => -1
                case None => 0
              }
          }.map {
            case WyzePlug(_, mac, nickname, maybe_is_on) =>
              val prefix = maybe_is_on match {
                case Some(true) =>
                  s"[ ] "
                case Some(false) =>
                  s"[x] "
                case None =>
                  ""
              }
              s"- $prefix[[Wyze plug $mac|$nickname]]"
          }.mkString("\n") + "\n"
      }
    }
  }

  private def genIsOnMap(plugs: List[WyzePlug]): Map[String, Boolean] = {
    plugs.flatMap {
      case WyzePlug(_, mac, _, maybe_is_on) =>
        maybe_is_on.map(mac -> _)
    }.toMap
  }
}
