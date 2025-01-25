package me.micseydel.actor.wyze

import me.micseydel.actor.VaultPathAdapter.VaultPathUpdatedEvent
import me.micseydel.actor.wyze.WyzePlugModel.{WyzePlug, WyzePlugAPIResponse, WyzePlugAPIResult}
import me.micseydel.actor.{ActorNotesFolderWatcherActor, VaultPathAdapter}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object WyzeActor {
  sealed trait Message

  private final case class ReceiveDeviceList(result: WyzePlugAPIResponse) extends Message

  private final case class ReceiveVaultPathUpdatedEvent(vaultPathUpdatedEvent: VaultPathUpdatedEvent) extends Message

  private val NoteName = "Wyze tinkering"

  def apply(wyzeUri: String)(implicit Tinker: Tinker): Ability[Message] = Tinkerer(TinkerColor.rgb(0, 255, 255), "🔌").setup { _ =>
    Tinker.initializedWithNote(NoteName, "_actor_notes/wyze") { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context

      val api = context.cast(WyzeAPIActor(wyzeUri), "WyzeAPIActor")
      api !! WyzeAPIActor.GetDevices(context.messageAdapter(ReceiveDeviceList))

      context.system.actorNotesFolderWatcherActor !! ActorNotesFolderWatcherActor.Subscribe("wyze", context.messageAdapter(ReceiveVaultPathUpdatedEvent))

      initializing(noteRef, api)
    }
  }

  private def initializing(noteRef: NoteRef, api: SpiritRef[WyzeAPIActor.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case ReceiveVaultPathUpdatedEvent(vaultPathUpdatedEvent) =>
        context.actorContext.log.warn(s"Not yet initialized, ignoring $vaultPathUpdatedEvent")
        Tinker.steadily

      case ReceiveDeviceList(wyzePlugAPIResponse) =>
        wyzePlugAPIResponse match {
          case WyzePlugModel.WyzePlugAPIResponseFailed(throwable) =>
            throw throwable

          case WyzePlugAPIResult(wyze_plug_list) =>
            noteRef.setMarkdown {
              s"- Markdown generated ${context.system.clock.now()}\n" +
                wyze_plug_list.map {
                  case WyzePlug(_, mac, nickname, maybe_is_on) =>
                    maybe_is_on match {
                      case Some(true) =>
                        s"- [ ] $nickname: $mac"
                      case Some(false) =>
                        s"- [x] $nickname: $mac"
                      case None =>
                        s"- $nickname: $mac"
                    }
                }.mkString("\n") + "\n"
            }

            val isOnMap = wyze_plug_list.flatMap {
              case WyzePlug(_, mac, _, maybe_is_on) =>
                maybe_is_on.map(mac -> _)
            }.toMap

            initialized(isOnMap)(Tinker, noteRef, api)
        }
    }
  }

  private val Off = 'x'
  private val On = ' '

  private def initialized(isOnMap: Map[String, Boolean])(implicit Tinker: Tinker, noteRef: NoteRef, api: SpiritRef[WyzeAPIActor.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
      case ReceiveVaultPathUpdatedEvent(event) =>
        event match {
          case VaultPathAdapter.PathModifiedEvent(_) =>
            context.actorContext.log.info("File updated, reading Markdown...")
            val isOnMapFromDisk = noteRef.readMarkdown() match {
              case Failure(exception) => throw exception
              case Success(markdown) =>
                markdown.split("\n")
                  .filter(_.startsWith("- ["))
                  .flatMap { line =>
                    val maybeOnOff: Option[Boolean] = line(3) match {
                      case Off => Some(false)
                      case On => Some(true)
                      case _ =>
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
            }

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

          case VaultPathAdapter.PathCreatedEvent(_) | VaultPathAdapter.PathDeletedEvent(_) =>
            Tinker.steadily
        }

      case ReceiveDeviceList(result) =>
        context.actorContext.log.warn(s"Did not expect to receive a device list, ignoring $result")
        Tinker.steadily
    }
  }
}
