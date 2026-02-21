package me.micseydel.app.selfsortingarrays.cell.atom

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Environment.InvariantViolation
import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.*
import me.micseydel.app.selfsortingarrays.cell.atom.Helper.InsertionSortCellRichNoteRef
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

object InsertionSortCellStateMachine {
  def initializing(index: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {
    val priorState = None
    val state = InsertionSortCellState(index, None, None)
    helper("initializing")(priorState, state) {
      case Initialize(leftNeighbor, rightNeighbor) =>
        val updatedState = InsertionSortCellState(index, leftNeighbor, rightNeighbor)
        noteRef.updateDocument("initialized", updatedState, "initialized", retainHistory = false)
        markdownListHistoryLine("initialized, waiting") ->
          InsertionSortCellStateMachine.waiting(state, updatedState)

      case _ => ???
    }
  }

  def waiting(priorState: InsertionSortCellState, state: InsertionSortCellState)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    helper("waiting")(Some(priorState), state) {
      case Initialize(leftNeighbor, rightNeighbor) => throw InvariantViolation(s"can't initialize, already initialized; leftNeighbor=$leftNeighbor, rightNeighbor=$rightNeighbor")
      case DoSort =>
        if (state.wantToSwapWithRight()) {
          markdownListHistoryLine(s"swapping right do to a DoSort; left neighbor = ${state.maybeLeftNeighbor.map(_.id)}") ->
            InsertionSortCellStateMachine.swappingRight(state, state, state.maybeLeftNeighbor, "DoSort->waiting")
        } else {
          state.maybeRightNeighbor.foreach(_ !~! DoSort)
          markdownListHistoryLine(s"DoSort propagated right ${state.maybeRightNeighbor.map(_.id)}") ->
            InsertionSortCellStateMachine.waiting(state, state)
        }
      case protocol: SwapProtocol =>
        protocol match {
          // FIXME: should this reject (or at least log) when a smaller number tries to begin a swap?
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
                state.maybeRightNeighbor.foreach(_ !~! NotifyOfSwap(Left(Some(oldLeftNeighbor)), self.id))
                oldLeftNeighbor !~! CompleteSwap(Right(state.maybeRightNeighbor), self.id)
                markdownListHistoryLine("begin swap resulted in waiting, decremented index") ->
                  InsertionSortCellStateMachine.waiting(state, updatedState)
            }
          case CompleteSwap(newRightOrReject, originator) =>
            newRightOrReject match {
              case Left(NoOp) => ???
              case Right(newRight) =>
                // this means we moved right
                val updatedState = state.copy(state.index + 1, state.maybeRightNeighbor, maybeRightNeighbor = newRight)
                state.maybeLeftNeighbor.foreach(_ !~! NotifyOfSwap(Right(state.maybeRightNeighbor), self.id))
                if (updatedState.wantToSwapWithRight()) {
                  markdownListHistoryLine(s"swapping right (index++) because of a complete swap($originator) (new right = ${updatedState.maybeRightId}), left neighbor=${state.maybeLeftId}") ->
                    InsertionSortCellStateMachine.swappingRight(state, updatedState, state.maybeLeftNeighbor, "CompleteSwap->waiting")
                } else {
                  markdownListHistoryLine(s"swapping right (index++, complete swap($originator)) resulted in waiting (${updatedState.maybeLeftId} | ${updatedState.maybeRightId})") ->
                    InsertionSortCellStateMachine.waiting(state, updatedState)
                }
            }
          case NotifyOfSwap(replacementLeftOrRight, originator) =>
            replacementLeftOrRight match {
              case Left(newMaybeLeftNeighbor) =>
                val updatedState = state.copy(maybeLeftNeighbor = newMaybeLeftNeighbor)
                markdownListHistoryLine(s"notified of swap now waiting, left=${updatedState.maybeLeftId} (previously ${state.maybeLeftId})") ->
                  InsertionSortCellStateMachine.waiting(state, updatedState)
              case Right(newMaybeRightNeighbor) =>
                if (state.maybeLeftNeighbor.flatMap(l => newMaybeRightNeighbor.map(l.id == _.id)).getOrElse(false)) {
                  throw InvariantViolation(s"[${self.id}] left/right conflict, told Notify(right=${newMaybeRightNeighbor.get.id}) by $originator (overwriting right=${state.maybeRightNeighbor.map(_.id)}) but left is already ${state.maybeLeftNeighbor.get.id} so FIGURE OUT HOW TO CHANGE BOTH, don't conflict!")
                }
                val updatedState = state.copy(maybeRightNeighbor = newMaybeRightNeighbor)
                if (updatedState.wantToSwapWithRight()) {
                  markdownListHistoryLine(s"notify of swap resulting in swapping right, newRight=${updatedState.maybeRightId}") ->
                    InsertionSortCellStateMachine.swappingRight(state, updatedState, state.maybeLeftNeighbor, s"NotifyOfSwap->waiting  state changed=${priorState == state}")
                } else {
                  markdownListHistoryLine("notify of swap resulted in waiting") ->
                    InsertionSortCellStateMachine.waiting(state, updatedState)
                }
            }
        }
      case ClockTick(count) =>
        markdownListHistoryLine(s"ignoring $count clock tick") -> Tinker.steadily
    }
  }


  // FIXME: waiting / prepped to swap right
  def swappingRight(priorState: InsertionSortCellState, state: InsertionSortCellState,
                    oldLeftNeighbor: Option[InsertionSortCellWrapper] // FIXME DEBUGGING is this necessary? can it be removed?
                    , from: String // for debugging
                   )(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = {


    // FIXME: hacking
    def debuggingHacking(rightNeighbor: InsertionSortCellWrapper, oldLeftNeighbor: Option[InsertionSortCellWrapper]): Unit = {
      if (self.id == 5) {
        if (rightNeighbor.id == 7 && oldLeftNeighbor.map(_.id).contains(1) && state.maybeLeftNeighbor.map(_.id).contains(6)) {
          println(s"[5] ❌ ${priorState.index == state.index} ${oldLeftNeighbor.map(_.id)} but it should send ${state.maybeLeftNeighbor.map(_.id)} - but why? (from:$from)")
        } else {
          println(s"[5] ✅? ${priorState.index == state.index} -${oldLeftNeighbor.map(_.id)}-> ${rightNeighbor.id} (did not send ${state.maybeLeftNeighbor.map(_.id)}, from:$from)")
        }
      } else {
        println(s"[${self.id}] ✅ ${priorState.index == state.index} -${oldLeftNeighbor.map(_.id)}-> ${rightNeighbor.id} (and ignoring ${state.maybeLeftNeighbor.map(_.id)}, from:$from)")
      }
    }

    // FIXME: fold waitingForClockTick into this method?
    waitingForClockTick(priorState, state,
      Tinker.setup { _ =>
        // after the clock ticks, we start the actual swapping
        state.maybeRightNeighbor match {
          case Some(rightNeighbor) =>
            // FIXME: it seems that state.maybeLeftNeighbor needs to be used instead of oldLeftNeighbor sometimes, but how to differentiate?
            debuggingHacking(rightNeighbor, oldLeftNeighbor)

            // FIXME it looks like this is sometimes using a cached value instead of a newer value
            //  - using state.maybeLeftNeighbor or priorState.leftNeighbor does not work either
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
                    // FIXME this is (probably) involved in the bug!
                    println(s"[${self.id}] CANARY left=${state.maybeLeftNeighbor.map(_.id)}; prior=${priorState.maybeLeftNeighbor.map(_.id)} and updated=${updatedState.maybeLeftNeighbor.map(_.id)}")
                    // FIXME DEBUGGING changing state.maybeLeftNeighbor to updatedState.maybeLeftNeighbor does not resolve the problem
                    markdownListHistoryLine(s"complete swap from $originator resulting in swapping right, newRight=${updatedState.maybeRightId}") ->
                      InsertionSortCellStateMachine.swappingRight(state, updatedState, state.maybeLeftNeighbor, "CompleteSwap->swappingRight") // trying to use updatedState here breaks things!
                  } else {
                    if (updatedState.locallySorted()) {
                      updatedState.maybeRightNeighbor.foreach(_ !~! DoSort)
                    }
                    markdownListHistoryLine(s"complete swap from $originator resulted in waiting") ->
                      InsertionSortCellStateMachine.waiting(state, updatedState)
                  }
              }
              case NotifyOfSwap(replacementLeftOrRight, originator) =>
                replacementLeftOrRight match {
                  case Left(newMaybeLeftNeighbor) =>
                    val updatedState = state.copy(maybeLeftNeighbor = newMaybeLeftNeighbor)
                    markdownListHistoryLine(s"notify of swap from $originator resulted in waiting") ->
                      InsertionSortCellStateMachine.waiting(state, updatedState)
                  case Right(newMaybeRightNeighbor) =>
                    throw InvariantViolation(s"Was mid-swap but $originator sent new Right=$newMaybeRightNeighbor")
                }
            }

          case Initialize(leftNeighbor, rightNeighbor) => throw InvariantViolation(s"can't initialize, already initialized; leftNeighbor=$leftNeighbor, rightNeighbor=$rightNeighbor")
          case DoSort => markdownListHistoryLine("ignoring DoSort") -> Tinker.steadily
          case ClockTick(count) => markdownListHistoryLine(s"ignoring clock tick $count") -> Tinker.steadily
        }
      }
    )
  }

  private def waitingForClockTick(priorState: InsertionSortCellState, state: InsertionSortCellState, newAbility: => Ability[Message])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, tinkerContext: TinkerContext[?], probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    state.sanityChecks(Some(priorState))

    helper("waitingForClockTick")(Some(priorState), state) {
      case BeginSwap(_, _) =>
        priorState.maybeLeftNeighbor.foreach(_ !~! CompleteSwap(Left(NoOp), self.id))
        markdownListHistoryLine("LINE NOT REACHED (?!)") -> Behaviors.same

      case DoSort => markdownListHistoryLine(s"ignoring DoSort (presumably from ${state.maybeLeftId})") -> Behaviors.same // can safely ignore?

      case ClockTick(count) =>
        markdownListHistoryLine(s"clock tick $count switching to swapping") -> newAbility

      case other =>
        markdownListHistoryLine(s"ignoring $other") ->
          Behaviors.same
    }
  }

  // helper

  private def helper(tag: String)(priorState: Option[InsertionSortCellState], state: InsertionSortCellState)(
    onMessage: Message => (String, Behavior[Message]) // FIXME: whatHappened documentation
  )(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = {
    Tinker.setup { context =>
      implicit val tc: TinkerContext[?] = context
      state.sanityChecks(priorState)
      Tinker.userExtension.probe !! state.probe

      Tinker.receiveMessage { message =>
        onMessage(message) match {
          case (historyString, newBehavior) =>
            val hackyTag = if (tag == "waiting" && state.locallySorted()) "sorted" else tag
            noteRef.updateDocument(hackyTag, state, historyString) match {
              case Failure(exception) => throw exception
              case Success(NoOp) =>
            }
            newBehavior
        }
      }
    }
  }

  private def markdownListHistoryLine(string: String)(implicit context: TinkerContext[?]): String = {
    s"- \\[${context.system.clock.now().toString.drop(11).dropRight(27)}] $string"
  }
}