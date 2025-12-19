package me.micseydel.app.selfsortingarrays.cell

import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.{Probe, SelfSortingArrays}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success, Try}

// loosely inspired from https://github.com/Zhangtaining/cell_research/blob/main/modules/Cell.py
// ...but with the actor model
// ??? is used in various places to crash if a surprise happens, but it may need to be replaced with Behaviors.same or Tinker.steadily if it's just a duplicate message and not a bug
object InsertionSortCell {

  // use this to allow Obsidian to create an animation
  //  private val SleepDurationMillis = 2000

  type InsertionSortCellWrapper = CellWrapper[Message]

  // inbox 📥

  sealed trait Message

  final case class Initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]) extends Message

  case object DoSort extends Message

  sealed trait SwapProtocol extends Message {
    def originator: Int
  }

  case class BeginSwap(newLeft: Option[InsertionSortCellWrapper], originator: Int) extends SwapProtocol

  case class CompleteSwap(newRight: Option[InsertionSortCellWrapper], originator: Int) extends SwapProtocol

  // for the non-participants to the swap
  case class NotifyOfSwap(replacementLeftOrRight: Either[Option[InsertionSortCellWrapper], Option[InsertionSortCellWrapper]], originator: Int) extends SwapProtocol

  case class ClockTick(count: Int) extends Message

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "📵") { (context, noteRef) =>
    implicit val self: InsertionSortCellWrapper = CellWrapper(id, value, noteName, context.self)
    implicit val nr: NoteRef = noteRef
    implicit val tc: TinkerContext[?] = context
    implicit val probe: SpiritRef[Probe.Message] = Tinker.userExtension.probe
    Tinker.userExtension.probe !! Probe.Register(id, noteName)
    StateMachine.initializing(index)
  }

  //

  private case class InvariantViolation(details: String) extends RuntimeException(details)

  case class CellState(index: Int, maybeLeftNeighbor: Option[InsertionSortCellWrapper], maybeRightNeighbor: Option[InsertionSortCellWrapper]) {
    def probe(implicit cw: InsertionSortCellWrapper): Probe.UpdatedState = Probe.UpdatedState(cw.id, this)

    def sanityChecks(maybePriorState: Option[CellState])(implicit self: InsertionSortCellWrapper, Tinker: EnhancedTinker[SelfSortingArrayCentralCast], tinkerContext: TinkerContext[?]): Unit = {
      if (maybeLeftNeighbor.map(_.id).contains(self.id)) {
        throw InvariantViolation(s"[${self.id}] Left neighbor ${maybeLeftNeighbor.get.id} is self!")
      }
      if (maybeRightNeighbor.map(_.id).contains(self.id)) {
        throw InvariantViolation(s"[${self.id}] right neighbor is self!")
      }

      for {
        leftNeighbor <- maybeLeftNeighbor
        rightNeighbor <- maybeRightNeighbor
      } if (leftNeighbor.id == rightNeighbor.id) {
        val msg = maybePriorState match {
          case Some(priorState) =>
            s"[${self.id}] Can't have left and right be the same: ${leftNeighbor.id}, prior state left&right: ${priorState.maybeLeftNeighbor.map(_.id)} & ${priorState.maybeRightNeighbor.map(_.id)}"
          case None =>
            s"[${self.id}] Can't have left and right be the same: ${leftNeighbor.id} (no prior state)"
        }
        Tinker.userExtension.probe !! Probe.FoundABug(msg)
        throw InvariantViolation(msg)
      }
    }

    def locallySorted()(implicit self: InsertionSortCellWrapper): Boolean = {
      maybeLeftNeighbor.forall(self.value >= _.value) &&
        maybeRightNeighbor.forall(self.value <= _.value)
    }

    def terseTransition(prior: CellState)(implicit cw: InsertionSortCellWrapper): String = {
      def f(t: Option[InsertionSortCellWrapper]): String = t.map(_.id.toString).getOrElse("x")

      s"${cw.id} (${f(prior.maybeLeftNeighbor)}, ${f(prior.maybeRightNeighbor)}) -> (${f(maybeLeftNeighbor)}, ${f(maybeRightNeighbor)})"
    }
  }

  // state machine

  private object StateMachine {
    def initializing(index: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] =
      stateMachine("initializing")(None, CellState(index, None, None))(new CellBehaviors {
        override def initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]): Ability[Message] =
          StateMachine.waiting(Some(CellState(index, None, None)), CellState(index, leftNeighbor, rightNeighbor))

        override def doSort(): Ability[Message] = ???

        override def clockTick(count: Int): Ability[Message] = Tinker.steadily

        override def beginSwap(state: CellState, priorState: Option[CellState], newLeft: Option[InsertionSortCellWrapper], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[_], probe: SpiritRef[Probe.Message]): Ability[Message] = ???

        override def completeSwap(state: CellState, priorState: Option[CellState], newRight: Option[InsertionSortCellWrapper], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[_], probe: SpiritRef[Probe.Message]): Ability[Message] = ???

        override def notifyOfSwap(state: CellState, priorState: Option[CellState], replacementLeftOrRight: Either[Option[InsertionSortCellWrapper], Option[InsertionSortCellWrapper]], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[_], probe: SpiritRef[Probe.Message]): Ability[Message] = ???
      })

    def waiting(priorState: Option[CellState], state: CellState)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] =
      stateMachine("waiting")(priorState, state) {
        class WaitingBehavior extends CellBehaviors with Initialized with DefaultOnCompleteSwap with DefaultBehavior {
          override def doSort(): Ability[Message] =
            state.maybeRightNeighbor match {
              case Some(rightNeighbor) =>
                if (self.value > rightNeighbor.value) {
                  StateMachine.swappingRight(priorState, state, state.maybeLeftNeighbor)
                } else {
                  rightNeighbor !~! DoSort
                  StateMachine.waitingOrSorted(priorState, state)
                }
              case None =>
                StateMachine.waitingOrSorted(priorState, state)
            }

          override def clockTick(count: Int): Ability[Message] = Tinker.steadily
        }
        new WaitingBehavior()
      }

    def waitingForClockTick(priorState: Option[CellState], state: CellState, newAbility: => Ability[Message])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?]): Ability[Message] = Tinker.setup { context =>
      state.sanityChecks(priorState)
      Tinker.setup { context =>
        Tinker.userExtension.probe !! state.probe

        noteRef.setRaw(
          s"""---
             |tags: [waitingForClockTick]
             |---
             |- start index: ${self.id}
             |- index: ${state.index}
             |
             |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}|${state.maybeRightNeighbor.map(_.noteName).map(s => s" [[$s]] ->").getOrElse("")}
             |""".stripMargin) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }

        Behaviors.withStash(10) { stash =>
          Tinker.receiveMessage {
            case ClockTick(count) =>
              stash.unstashAll(newAbility)

            case other =>
              stash.stash(other)
              Tinker.steadily
          }
        }

      }
    }

    def swappingRight(priorState: Option[CellState], state: CellState, oldLeftNeighbor: Option[InsertionSortCellWrapper])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = waitingForClockTick(priorState, state,
      Tinker.setup { _ =>
        state.maybeRightNeighbor.foreach(_ !~! BeginSwap(oldLeftNeighbor, self.id))
        stateMachine("swappingRight")(priorState, state) {
          class SwappingRightBehavior extends CellBehaviors with Initialized with DoesNotSort with DefaultBehavior {
            override def beginSwap(state: CellState, priorState: Option[CellState], newLeft: Option[InsertionSortCellWrapper], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] =
              throw InvariantViolation(s"[swappingRight ${self.id}] unexpected message from $originator: newLeft=$newLeft")

            override def completeSwap(state: CellState, priorState: Option[CellState], newRight: Option[InsertionSortCellWrapper], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {
              // this means we moved right
              val updatedState = state.copy(state.index + 1, state.maybeRightNeighbor, maybeRightNeighbor = newRight)
              state.maybeLeftNeighbor.foreach(_ !~! NotifyOfSwap(Right(state.maybeRightNeighbor), self.id))
              /// expansion-for-tinkering of:
//              StateMachine.swappingOrPassive(updatedState, state.maybeLeftNeighbor)
              updatedState.maybeRightNeighbor match {
                case Some(rightNeighbor) if rightNeighbor.value < self.value =>
                  StateMachine.swappingRight(priorState, updatedState, state.maybeLeftNeighbor)
                case Some(_) | None =>

//                  StateMachine.waitingOrSorted(updatedState)
                  // above line replaced with a copy-paste for tinkering (if ... sorted else waiting)

                  if (updatedState.locallySorted()) {
                    // FIXME: testing with fixing the sorting bug
//                    state.maybeRightNeighbor.foreach(_ !! DoSort)
                    sorted(Some(state), updatedState)
                  } else {
                    // FIXME: testing with fixing the sorting bug
//                    state.maybeRightNeighbor.foreach(_ !! DoSort)
                    waiting(Some(state), updatedState)
                  }
              }
            }

            override def clockTick(count: Int): Ability[Message] = Tinker.steadily
          }
          new SwappingRightBehavior()
        }
      })

    def sorted(priorState: Option[CellState], state: CellState)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] =

      Tinker.setup { context =>
        // FIXME: experiment- should this tell downstream to start sorting itself?

        stateMachine("sorted")(priorState, state) {
          class SortedBehavior extends CellBehaviors with Initialized with DoesNotSort with DefaultBehavior with DefaultOnCompleteSwap {
            override def clockTick(count: Int): Ability[Message] = Tinker.steadily
          }
          new SortedBehavior()
        }
      }

    def waitingOrSorted(priorState: Option[CellState], state: CellState)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {
      if (state.locallySorted()) {
        sorted(priorState, state)
      } else {
        waiting(priorState, state)
      }
    }

    def swappingOrPassive(priorState: Option[CellState], state: CellState, maybeLeftNeighbor: Option[InsertionSortCellWrapper])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {
      state.maybeRightNeighbor match {
        case Some(rightNeighbor) if rightNeighbor.value < self.value =>
          StateMachine.swappingRight(priorState, state, maybeLeftNeighbor)
        case Some(_) | None =>
          StateMachine.waitingOrSorted(priorState, state)
      }
    }

    // helper

    private def stateMachine(tag: String)(priorState: Option[CellState], state: CellState)(behaviors: CellBehaviors)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = {
      Tinker.setup { context =>
        implicit val tc: TinkerContext[?] = context
        state.sanityChecks(priorState)
        Tinker.userExtension.probe !! state.probe

        noteRef.setRaw(
          s"""---
             |tags: [$tag]
             |---
             |- start index: ${self.id}
             |- index: ${state.index}
             |
             |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}|${state.maybeRightNeighbor.map(_.noteName).map(s => s" [[$s]] ->").getOrElse("")}
             |""".stripMargin) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }

        Tinker.receiveMessage(behaviors.ability(priorState, state))
      }
    }
  }

  private abstract class CellBehaviors {
    def ability(priorState: Option[CellState], state: CellState)(message: Message)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = (message match {
      case Initialize(leftNeighbor, rightNeighbor) => initialize(leftNeighbor, rightNeighbor)
      case DoSort => doSort()
      case protocol: SwapProtocol =>
        protocol match {
          case BeginSwap(newLeft, originator) => beginSwap(state, priorState, newLeft, originator)
          case CompleteSwap(newRight, originator) => completeSwap(state, priorState, newRight, originator)
          case NotifyOfSwap(replacementLeftOrRight, originator) => notifyOfSwap(state, priorState, replacementLeftOrRight, originator)
        }

      case ClockTick(count) => clockTick(count)
    })

    def initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]): Ability[Message]

    def doSort(): Ability[Message]

    def beginSwap(state: CellState, priorState: Option[CellState], newLeft: Option[InsertionSortCellWrapper], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message]

    def completeSwap(state: CellState, priorState: Option[CellState], newRight: Option[InsertionSortCellWrapper], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message]

    def notifyOfSwap(state: CellState, priorState: Option[CellState], replacementLeftOrRight: Either[Option[InsertionSortCellWrapper], Option[InsertionSortCellWrapper]], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message]

    def clockTick(count: Int): Ability[Message]
  }

  // mixins

  private trait Initialized {
    def initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]): Ability[Message] =
      throw InvariantViolation(s"can't initialize, already initialized; leftNeighbor=$leftNeighbor, rightNeighbor=$rightNeighbor")
  }

  private trait DoesNotSort {
    def doSort(): Ability[Message] =
      throw InvariantViolation(s"state does not support sorting")
  }

  private trait DefaultOnCompleteSwap {
    def completeSwap(state: CellState, priorState: Option[CellState], newRight: Option[InsertionSortCellWrapper], originator: Int)
                    (implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {
      // this means we moved right
      val updatedState = state.copy(state.index + 1, state.maybeRightNeighbor, maybeRightNeighbor = newRight)
      state.maybeLeftNeighbor.foreach(_ !~! NotifyOfSwap(Right(state.maybeRightNeighbor), self.id))
      //      StateMachine.waitingOrSorted(updatedState) // FIXME
      StateMachine.swappingOrPassive(Some(state), updatedState, state.maybeLeftNeighbor)
    }
  }

  private trait DefaultBehavior {
    def beginSwap(state: CellState, priorState: Option[CellState], newLeft: Option[InsertionSortCellWrapper], originator: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[_], probe: SpiritRef[Probe.Message]): Ability[Message] = {
      state.sanityChecks(priorState)
      // our left neighbor is trying to swap with THIS; it's moving right, THIS is moving left
      // - THIS is receiving a replacement (or None) left neighbor
      // - then notifying OLD right
      state.maybeLeftNeighbor match {
        case None =>
          val msg = s"\\[${self.id}] Was told to begin swap but left neighbor is empty, ignoring newLeft=$newLeft from $originator"
          Tinker.userExtension.probe !! Probe.FoundABug(msg)
          throw InvariantViolation(msg)
        case Some(oldLeftNeighbor) =>
          if (newLeft.map(_.id).contains(oldLeftNeighbor.id)) {
            val msg = s"\\[${self.id}] was told to BeginSwap(newLeft=${newLeft.map(_.id)}) by $originator BUT existing left=${oldLeftNeighbor.id}"
            Tinker.userExtension.probe !! Probe.FoundABug(msg)
            throw InvariantViolation(msg)
          }
          // we're changing our left neighbor,
          // meaning our left neighbor is taking our place and our new left neighbor is their old left neighbor,
          // so we let our OLD left neighbor know
          val updatedState = state.copy(state.index - 1, maybeLeftNeighbor = newLeft, maybeRightNeighbor = Some(oldLeftNeighbor))
          Try(updatedState.sanityChecks(Some(state))) match {
            case Failure(exception: InvariantViolation) =>
              val msg = s"[sorted ${self.id}] ${exception.details} (caused by BeginSwap(newLeft=${newLeft.map(_.id)}) from $originator, prior state left&right: ${state.maybeLeftNeighbor.map(_.id)} & ${state.maybeRightNeighbor.map(_.id)})"
              throw InvariantViolation(msg)
            case Failure(e) => throw e
            case Success(_) =>
          }
          state.maybeRightNeighbor.foreach(_ !~! NotifyOfSwap(Left(Some(oldLeftNeighbor)), self.id))
          oldLeftNeighbor !~! CompleteSwap(state.maybeRightNeighbor, self.id)
          StateMachine.waitingOrSorted(Some(state), updatedState)
      }
    }

    def notifyOfSwap(state: CellState, priorState: Option[CellState], replacementLeftOrRight: Either[Option[InsertionSortCellWrapper], Option[InsertionSortCellWrapper]], originator: Int)
                    (implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {
      state.sanityChecks(priorState)
      replacementLeftOrRight match {
        case Left(newMaybeLeftNeighbor) =>
          val updatedState = state.copy(maybeLeftNeighbor = newMaybeLeftNeighbor)
          StateMachine.waitingOrSorted(Some(state), updatedState)
        // THIS IS ALLOWED TO TRIGGER DOWNSTREAM SORTING
        //          StateMachine.swappingOrPassive(updatedState, state.maybeLeftNeighbor)
        case Right(newMaybeRightNeighbor) =>
          if (state.maybeLeftNeighbor.flatMap(l => newMaybeRightNeighbor.map(l.id == _.id)).getOrElse(false)) {
            throw InvariantViolation(s"[${self.id}] left/right conflict, told Notify(right=${newMaybeRightNeighbor.get.id}) by $originator (overwriting right=${state.maybeRightNeighbor.map(_.id)}) but left is already ${state.maybeLeftNeighbor.get.id} so FIGURE OUT HOW TO CHANGE BOTH, don't conflict!")
          }
          val updatedState = state.copy(maybeRightNeighbor = newMaybeRightNeighbor)
          for {
            l <- updatedState.maybeLeftNeighbor
            r <- updatedState.maybeRightNeighbor
          } if (l.id == r.id) {
            println(updatedState)
            ???
          }
          updatedState.sanityChecks(priorState)
          //          StateMachine.waiting(updatedState)
          StateMachine.swappingOrPassive(priorState, updatedState, state.maybeLeftNeighbor)
      }
    }
  }
}
