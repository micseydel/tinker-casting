package me.micseydel.actor.hue

import me.micseydel.actor.notifications.ChimeActor.Material
import me.micseydel.actor.notifications.NotificationCenterManager.JustSideEffect
import me.micseydel.actor.notifications.{ChimeActor, NotificationCenterManager}
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerContext}
import me.micseydel.model.*
import me.micseydel.vault.NoteId

import java.time.LocalDate
import scala.concurrent.duration.DurationInt

private[hue] object HueListerHelperForNote {
  sealed trait Message

  final case class ReceiveNoteInfo(forDay: LocalDate, whisperModel: WhisperModel, lightState: LightState, confidence: Double) extends Message
  final case class GoNoGo(noteId: NoteId, forDay: LocalDate, model: WhisperModel, decision: Either[String, String], deferred: LightState) extends Message

  //

  def apply(noteId: NoteId, manager: SpiritRef[HueListener.ReceiveDelegated])(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    def helper(lightState: LightState, confidence: Double, model: WhisperModel, forDay: LocalDate): Ability[HelperForModel.Message] =
      HelperForModel(noteId, confidence, lightState, context.self, VotingTimeout, model, forDay)

    def waiting(baseHasAlreadyFinished: Boolean = false): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case ReceiveNoteInfo(forDay, whisperModel, deferring: LightState, confidence) =>
          whisperModel match {
            case BaseModel if !baseHasAlreadyFinished =>
              context.cast(helper(deferring, confidence, whisperModel, forDay), "HelperForBase")
              processingBase()
            case LargeModel =>
              context.cast(helper(deferring, confidence, whisperModel, forDay), "HelperForLarge")
              processingLarge()
            case other =>
              if (baseHasAlreadyFinished) {
                context.actorContext.log.warn(s"Ignoring unexpected WhisperModel $other, only expected LargeModel")
              } else {
                context.actorContext.log.warn(s"Ignoring unexpected WhisperModel $other, only expected BaseModel or LargeModel")
              }
              Tinker.steadily
          }

        case premature@GoNoGo(_, _, _, _, _) =>
          context.actorContext.log.warn(s"Did not expect premature message $premature")
          Tinker.steadily
      }
    }

    def processingBase(): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case m@ReceiveNoteInfo(_, BaseModel, _, _) =>
          context.actorContext.log.warn(s"Ignoring $m, already processing BaseModel")
          Tinker.steadily
        case ReceiveNoteInfo(forDay, model@LargeModel, deferring: LightState, confidence) =>
          context.cast(HelperForModel(noteId, confidence, deferring, context.self, 30.seconds, model, forDay), "HelperForLarge")
          processingBoth()
        case ReceiveNoteInfo(_, otherModel, _, _) =>
          context.actorContext.log.warn(s"Was not expecting other model $otherModel, only expected BaseModel or LargeModel")
          Tinker.steadily
        case goNoGo@GoNoGo(noteId, forDay, model@BaseModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"Forwarding $deferred for $model because: $reasonToGo")
              manager !! HueListener.ReceiveDelegated(noteId, forDay, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"Blocking $deferred for $model because: $reasonToBlock")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          waiting(baseHasAlreadyFinished = true)
        case goNoGo@GoNoGo(_, _, _, _, _) =>
          context.actorContext.log.warn(s"Ignoring $goNoGo for unexpected model")
          Tinker.steadily
      }
    }

    def processingLarge(baseHasAlreadyFinished: Boolean = false): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case ReceiveNoteInfo(forDay, BaseModel, deferred: LightState, confidence) if !baseHasAlreadyFinished =>
          // could probably just ignore this, voting should be fast...
          context.cast(HelperForModel(noteId, confidence, deferred, context.self, 250.milliseconds, BaseModel, forDay), "HelperForBase")
          processingBoth()
        case m@ReceiveNoteInfo(_, LargeModel, _, _) =>
          context.actorContext.log.warn(s"Ignoring $m, already processing LargeModel")
          Tinker.steadily
        case ReceiveNoteInfo(_, otherModel, _, _) =>
          if (baseHasAlreadyFinished) {
            context.actorContext.log.warn(s"Was not expecting other model $otherModel, only expected LargeModel (BaseModel already finished)")
          } else {
            context.actorContext.log.warn(s"Was not expecting other model $otherModel, only expected BaseModel or LargeModel")
          }
          Tinker.steadily
        case goNoGo@GoNoGo(noteId, forDay, model@LargeModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"Forwarding $deferred for $LargeModel because: $reasonToGo")
              manager !! HueListener.ReceiveDelegated(noteId, forDay, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"Blocking $deferred for $LargeModel because: $reasonToBlock")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          Tinker.done
        case GoNoGo(_, _, otherModel, decision, _) =>
          context.actorContext.log.warn(s"Received go/nogo ($decision) for $otherModel (expected LargeModel), ignoring")
          Tinker.steadily
      }
    }

    def processingBoth(): Ability[Message] = Tinker.setup { context =>
      Tinker.receiveMessage {
        case m@ReceiveNoteInfo(_, _, _, _) =>
          context.actorContext.log.warn(s"Already processing base and large, ignoring $m")
          Tinker.steadily
        case goNoGo@GoNoGo(_, forDay, model@BaseModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"GOing for $model $noteId: $reasonToGo")
              manager !! HueListener.ReceiveDelegated(noteId, forDay, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"NO go for $model $noteId: $reasonToBlock")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          processingLarge(baseHasAlreadyFinished = true)
        case goNoGo@GoNoGo(_, forDay, model@LargeModel, _, deferred) =>
          goNoGo(
            onGo = { reasonToGo =>
              context.actorContext.log.info(s"GOing for $model $noteId: $reasonToGo, DONE")
              manager !! HueListener.ReceiveDelegated(noteId, forDay, deferred, model)
            },
            onNoGo = { reasonToBlock =>
              context.actorContext.log.info(s"NO go for $model $noteId: $reasonToBlock, DONE")
              context.system.notifier !! JustSideEffect(NotificationCenterManager.Chime(ChimeActor.Success(Material)))
            }
          )
          Tinker.done
        case GoNoGo(_, _, otherModel, decision, _) =>
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
