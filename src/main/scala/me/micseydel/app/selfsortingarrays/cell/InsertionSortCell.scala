package me.micseydel.app.selfsortingarrays.cell

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Environment.InvariantViolation
import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success, Try}

// loosely inspired from https://github.com/Zhangtaining/cell_research/blob/main/modules/Cell.py
// ...but with the actor model
// ??? is used in various places to crash if a surprise happens, but it may need to be replaced with Behaviors.same or Tinker.steadily if it's just a duplicate message and not a bug
object InsertionSortCell {

  type InsertionSortCellWrapper = CellWrapper[Message]

  // inbox 📥

  sealed trait Message

  final case class Initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]) extends Message

  case object DoSort extends Message

  sealed trait SwapProtocol extends Message {
    def originator: Int
  }

  case class BeginSwap(newLeft: Option[InsertionSortCellWrapper], originator: Int) extends SwapProtocol

  case class CompleteSwap(newRightOrReject: Either[NoOp.type, Option[InsertionSortCellWrapper]], originator: Int) extends SwapProtocol

  // for the non-participants to the swap
  case class NotifyOfSwap(replacementLeftOrRight: Either[Option[InsertionSortCellWrapper], Option[InsertionSortCellWrapper]], originator: Int) extends SwapProtocol

  case class ClockTick(count: Int) extends Message

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "📵") { (context, noteRef) =>
    implicit val self: InsertionSortCellWrapper = CellWrapper(id, value, noteName, context.self)
    implicit val nr: NoteRef = noteRef
    implicit val tc: TinkerContext[?] = context
    implicit val probe: SpiritRef[Probe.Message] = Tinker.userExtension.probe
    Tinker.userExtension.probe !! Probe.Register(id, value, noteName)
    StateMachine.initializing(index)
  }

  //

  case class CellState(index: Int, maybeLeftNeighbor: Option[InsertionSortCellWrapper], maybeRightNeighbor: Option[InsertionSortCellWrapper]) {
    def probe(implicit cw: InsertionSortCellWrapper): Probe.UpdatedState = Probe.UpdatedState(cw.id, this)

    def wantToSwapWithRight()(implicit self: InsertionSortCellWrapper): Boolean = {
      maybeRightNeighbor.exists(_.value < self.value)
    }

    // FIXME: create a changeState (or something) method, which returns the new state and does the sanity checks automatically
    //  - updateneighbor leftorright
    //  - OR move (left or right)

    def sanityChecks(maybePriorState: Option[CellState])(implicit self: InsertionSortCellWrapper, Tinker: EnhancedTinker[SelfSortingArrayCentralCast], tinkerContext: TinkerContext[?]): Unit = {
      // we can't be our own neighbor
      if (maybeLeftNeighbor.map(_.id).contains(self.id)) {
        throw InvariantViolation(s"[${self.id}] Left neighbor ${maybeLeftNeighbor.get.id} is self!")
      }
      if (maybeRightNeighbor.map(_.id).contains(self.id)) {
        throw InvariantViolation(s"[${self.id}] right neighbor is self!")
      }

      // if we have two neighbors, they can't be the same
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
    def initializing(index: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {
      val priorState = None
      val state = CellState(index, None, None)
      helper("initializing")(priorState, state) {
        case Initialize(leftNeighbor, rightNeighbor) => StateMachine.waiting(CellState(index, None, None), CellState(index, leftNeighbor, rightNeighbor))

        case _ => ???
      }
    }

    def waiting(priorState: CellState, state: CellState)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
      helper("waiting")(Some(priorState), state) {
        case Initialize(leftNeighbor, rightNeighbor) => throw InvariantViolation(s"can't initialize, already initialized; leftNeighbor=$leftNeighbor, rightNeighbor=$rightNeighbor")
        case DoSort =>
          if (state.wantToSwapWithRight()) {
            StateMachine.swappingRight(state, state, /*FIXME: why is this state rather than prior state?!*/ state.maybeLeftNeighbor, "DoSort->waiting")
          } else {
            state.maybeRightNeighbor.foreach(_ !~! DoSort)
            StateMachine.waiting(state, state)
          }
        case protocol: SwapProtocol =>
          protocol match {
            case BeginSwap(newLeft, originator) =>
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
                  oldLeftNeighbor !~! CompleteSwap(Right(state.maybeRightNeighbor), self.id)
                  StateMachine.waiting(state, updatedState)
              }
            case CompleteSwap(newRightOrReject, originator) =>
              newRightOrReject match {
                case Left(NoOp) => ???
                case Right(newRight) =>
                  // this means we moved right
                  val updatedState = state.copy(state.index + 1, state.maybeRightNeighbor, maybeRightNeighbor = newRight)
                  state.maybeLeftNeighbor.foreach(_ !~! NotifyOfSwap(Right(state.maybeRightNeighbor), self.id))
                  if (updatedState.wantToSwapWithRight()) {
                    StateMachine.swappingRight(state, updatedState, state.maybeLeftNeighbor, "CompleteSwap->waiting")
                  } else {
                    StateMachine.waiting(state, updatedState)
                  }
              }
            case NotifyOfSwap(replacementLeftOrRight, originator) =>
              replacementLeftOrRight match {
                case Left(newMaybeLeftNeighbor) =>
                  val updatedState = state.copy(maybeLeftNeighbor = newMaybeLeftNeighbor)
                  StateMachine.waiting(state, updatedState)
                case Right(newMaybeRightNeighbor) =>
                  if (state.maybeLeftNeighbor.flatMap(l => newMaybeRightNeighbor.map(l.id == _.id)).getOrElse(false)) {
                    throw InvariantViolation(s"[${self.id}] left/right conflict, told Notify(right=${newMaybeRightNeighbor.get.id}) by $originator (overwriting right=${state.maybeRightNeighbor.map(_.id)}) but left is already ${state.maybeLeftNeighbor.get.id} so FIGURE OUT HOW TO CHANGE BOTH, don't conflict!")
                  }
                  val updatedState = state.copy(maybeRightNeighbor = newMaybeRightNeighbor)
                  updatedState.sanityChecks(Some(state)) // FIXME: should be able to remove
                  if (updatedState.wantToSwapWithRight()) {
                    StateMachine.swappingRight(state, updatedState, state.maybeLeftNeighbor, s"NotifyOfSwap->waiting  state changed=${priorState == state}")
                  } else {
                    StateMachine.waiting(state, updatedState)
                  }
              }
          }
        case ClockTick(count) => Tinker.steadily
      }
    }

    def swappingRight(priorState: CellState, state: CellState, oldLeftNeighbor: Option[InsertionSortCellWrapper]
                      , from: String
                     )(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] =
      waitingForClockTick(priorState, state, // FIXME
        Tinker.setup { _ =>
          // after the clock ticks, we start the actual swapping
          state.maybeRightNeighbor match {
            case Some(rightNeighbor) =>
            // FIXME: it seems that state.maybeLeftNeighbor needs to be used instead of oldLeftNeighbor sometimes, but how to differentiate?
            if (self.id == 5) {
              // FIXME: hacking
              if (rightNeighbor.id == 7 && oldLeftNeighbor.map(_.id).contains(1) && state.maybeLeftNeighbor.map(_.id).contains(6)) {
                println(s"[5] ❌ ${priorState.index == state.index} ${oldLeftNeighbor.map(_.id)} but it should send ${state.maybeLeftNeighbor.map(_.id)} - but why? (from:$from)")
              } else {
                println(s"[5] ✅? ${priorState.index == state.index} -${oldLeftNeighbor.map(_.id)}-> ${rightNeighbor.id} (did not send ${state.maybeLeftNeighbor.map(_.id)}, from:$from)")
              }
            } else {
              println(s"[${self.id}] ✅ ${priorState.index == state.index} -${oldLeftNeighbor.map(_.id)}-> ${rightNeighbor.id} (and ignoring ${state.maybeLeftNeighbor.map(_.id)}, from:$from)")
            }
              // FIXME is it a TIMING issue?
            rightNeighbor !~! BeginSwap(oldLeftNeighbor, self.id)

            case None => ??? // 😬
          }

          helper("swappingRight")(Some(priorState), state) {
            case protocol: SwapProtocol =>
              protocol match {
                case BeginSwap(newLeft, originator) => throw InvariantViolation(s"[swappingRight ${self.id}] unexpected message from $originator: newLeft=$newLeft")
                case CompleteSwap(newRightOrReject, originator) => newRightOrReject match {
                  case Left(NoOp) => ???
                  case Right(newRight) =>
                    // this means we moved right
                    val updatedState = state.copy(state.index + 1, state.maybeRightNeighbor, maybeRightNeighbor = newRight)
                    // our old left needs to know that their new right is our old right
                    state.maybeLeftNeighbor.foreach(_ !~! NotifyOfSwap(Right(state.maybeRightNeighbor), self.id))

                    if (updatedState.wantToSwapWithRight()) {
                      // FIXME this is involved in the bug!
                      println(s"[${self.id}] CANARY left=${state.maybeLeftNeighbor.map(_.id)}; prior=${priorState.maybeLeftNeighbor.map(_.id)} and updated=${updatedState.maybeLeftNeighbor.map(_.id)}")
                      StateMachine.swappingRight(state, updatedState, state.maybeLeftNeighbor, "CompleteSwap->swappingRight") // trying to use updatedState here breaks things!
                    } else {
                      if (updatedState.locallySorted()) {
                        updatedState.maybeRightNeighbor.foreach(_ !~! DoSort)
                      }
                      StateMachine.waiting(state, updatedState)
                    }
                }
                case NotifyOfSwap(replacementLeftOrRight, originator) =>
                  replacementLeftOrRight match {
                    case Left(newMaybeLeftNeighbor) =>
                      val updatedState = state.copy(maybeLeftNeighbor = newMaybeLeftNeighbor)
                      StateMachine.waiting(state, updatedState)
                    case Right(newMaybeRightNeighbor) =>
                      throw InvariantViolation(s"Was mid-swap but $originator sent new Right=$newMaybeRightNeighbor")
                  }
              }

            case Initialize(leftNeighbor, rightNeighbor) => throw InvariantViolation(s"can't initialize, already initialized; leftNeighbor=$leftNeighbor, rightNeighbor=$rightNeighbor")
            case DoSort => Tinker.steadily
            case ClockTick(count) => Tinker.steadily
          }
        })

    def waitingForClockTick(priorState: CellState, state: CellState, newAbility: => Ability[Message])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
      state.sanityChecks(Some(priorState))

      helper("waitingForClockTick")(Some(priorState), state) { m =>
        Behaviors.withStash(10) { stash =>
          m match {
            case BeginSwap(_, _) =>
              priorState.maybeLeftNeighbor.foreach(_ !~! CompleteSwap(Left(NoOp), self.id))
              Behaviors.same

            case DoSort => Behaviors.same // can safely ignore? FIXME

            case ClockTick(count) =>
              if (stash.size > 0) {
                println(s"unstashing ${stash.size} for clock tick $count")
              }
              stash.unstashAll(newAbility)

            case other =>
              println(s"STASHING $other")
              stash.stash(other)
              Behaviors.same
          }
        }
      }
    }

    // helper

    private def helper(tag: String)(priorState: Option[CellState], state: CellState)(onMessage: Message => Behavior[Message])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = {
      Tinker.setup { context =>
        implicit val tc: TinkerContext[?] = context
        state.sanityChecks(priorState)
        Tinker.userExtension.probe !! state.probe

        val hackyTag = if (tag == "waiting" && state.locallySorted()) "sorted" else tag
        noteRef.setRaw(
          s"""---
             |tags: [$hackyTag]
             |---
             |- start index: ${self.id}
             |- index: ${state.index}
             |
             |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}|${state.maybeRightNeighbor.map(_.noteName).map(s => s" [[$s]] ->").getOrElse("")}
             |""".stripMargin) match {
          case Failure(exception) => throw exception
          case Success(NoOp) =>
        }

        Tinker.receiveMessage(onMessage)
      }
    }
  }
}
