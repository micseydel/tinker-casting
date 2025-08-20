package me.micseydel.actor.hue

import me.micseydel.actor.notifications.ChimeActor.Material
import me.micseydel.actor.notifications.NotificationCenterManager.JustSideEffect
import me.micseydel.actor.notifications.{ChimeActor, NotificationCenterManager}
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerContext}
import me.micseydel.model.*
import me.micseydel.vault.NoteId

import scala.concurrent.duration.DurationInt

private[hue] object HueListerHelperForNote {
  sealed trait Message

  final case class ReceiveNoteInfo(whisperModel: WhisperModel, lightState: LightState, confidence: Double) extends Message
  final case class GoNoGo(noteId: NoteId, model: WhisperModel, decision: Either[String, String], deferred: LightState) extends Message

  //

  def apply(noteId: NoteId, manager: SpiritRef[HueListener.ReceiveDelegated])(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    def helper(lightState: LightState, confidence: Double, model: WhisperModel): Ability[HelperForModel.Message] =
      HelperForModel(noteId, confidence, lightState, context.self, VotingTimeout, model)

    def waiting(baseHasAlreadyFinished: Boolean = false): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case ReceiveNoteInfo(whisperModel, deferring: LightState, confidence) =>
          whisperModel match {
            case BaseModel if !baseHasAlreadyFinished =>
              context.cast(helper(deferring, confidence, whisperModel), "HelperForBase")
              processingBase()
            case LargeModel =>
              context.cast(helper(deferring, confidence, whisperModel), "HelperForLarge")
              processingLarge()
            case other =>
              if (baseHasAlreadyFinished) {
                context.actorContext.log.warn(s"Ignoring unexpected WhisperModel $other, only expected LargeModel")
              } else {
                context.actorContext.log.warn(s"Ignoring unexpected WhisperModel $other, only expected BaseModel or LargeModel")
              }
              Tinker.steadily
          }

        case premature@GoNoGo(_, _, _, _) =>
          context.actorContext.log.warn(s"Did not expect premature message $premature")
          Tinker.steadily
      }
    }

    def processingBase(): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case m@ReceiveNoteInfo(BaseModel, _, _) =>
          context.actorContext.log.warn(s"Ignoring $m, already processing BaseModel")
          Tinker.steadily
        case ReceiveNoteInfo(model@LargeModel, deferring: LightState, confidence) =>
          context.cast(HelperForModel(noteId, confidence, deferring, context.self, 30.seconds, model), "HelperForLarge")
          processingBoth()
        case ReceiveNoteInfo(otherModel, _, _) =>
          context.actorContext.log.warn(s"Was not expecting other model $otherModel, only expected BaseModel or LargeModel")
          Tinker.steadily
        case goNoGo@GoNoGo(noteId, model@BaseModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"Forwarding $deferred for $model because: $reasonToGo")
              manager !! HueListener.ReceiveDelegated(noteId, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"Blocking $deferred for $model because: $reasonToBlock")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          waiting(baseHasAlreadyFinished = true)
        case goNoGo@GoNoGo(_, _, _, _) =>
          context.actorContext.log.warn(s"Ignoring $goNoGo for unexpected model")
          Tinker.steadily
      }
    }

    def processingLarge(baseHasAlreadyFinished: Boolean = false): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case ReceiveNoteInfo(BaseModel, deferred: LightState, confidence) if !baseHasAlreadyFinished =>
          // could probably just ignore this, voting should be fast...
          context.cast(HelperForModel(noteId, confidence, deferred, context.self, 250.milliseconds, BaseModel), "HelperForBase")
          processingBoth()
        case m@ReceiveNoteInfo(LargeModel, _, _) =>
          context.actorContext.log.warn(s"Ignoring $m, already processing LargeModel")
          Tinker.steadily
        case ReceiveNoteInfo(otherModel, _, _) =>
          if (baseHasAlreadyFinished) {
            context.actorContext.log.warn(s"Was not expecting other model $otherModel, only expected LargeModel (BaseModel already finished)")
          } else {
            context.actorContext.log.warn(s"Was not expecting other model $otherModel, only expected BaseModel or LargeModel")
          }
          Tinker.steadily
        case goNoGo@GoNoGo(noteId, model@LargeModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"Forwarding $deferred for $LargeModel because: $reasonToGo")
              manager !! HueListener.ReceiveDelegated(noteId, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"Blocking $deferred for $LargeModel because: $reasonToBlock")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          Tinker.done
        case GoNoGo(_, otherModel, decision, _) =>
          context.actorContext.log.warn(s"Received go/nogo ($decision) for $otherModel (expected LargeModel), ignoring")
          Tinker.steadily
      }
    }

    def processingBoth(): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case m@ReceiveNoteInfo(_, _, _) =>
          context.actorContext.log.warn(s"Already processing base and large, ignoring $m")
          Tinker.steadily
        case goNoGo@GoNoGo(_, model@BaseModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"GOing for $model $noteId: $reasonToGo")
              manager !! HueListener.ReceiveDelegated(noteId, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"NO go for $model $noteId: $reasonToBlock")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          processingLarge(baseHasAlreadyFinished = true)
        case goNoGo@GoNoGo(_, model@LargeModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"GOing for $model $noteId: $reasonToGo, DONE")
              manager !! HueListener.ReceiveDelegated(noteId, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"NO go for $model $noteId: $reasonToBlock, DONE")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          Tinker.done
        case GoNoGo(_, otherModel, decision, _) =>
          context.actorContext.log.warn(s"Received go/nogo ($decision) for $otherModel (expected BaseModel or LargeModel), ignoring")
          Tinker.steadily
      }
    }

    waiting()
  }

  private val VotingTimeout = 250.milliseconds

  private implicit class RichGoNoGo(val goNoGo: GoNoGo) extends AnyVal {
    def apply(onGo: String => Unit, onNoGo: String => Unit): Unit = {
      goNoGo.decision match {
        case Left(noGo) => onNoGo(noGo)
        case Right(go) => onGo(go)
      }
    }
  }
}
