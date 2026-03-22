package me.micseydel.app.selfsortingarrays.cell

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Environment.InvariantViolation
import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.Probe.UpdatedState
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.cell.atom.Helper.InsertionSortCellRichNoteRef
import me.micseydel.app.selfsortingarrays.cell.atom.InsertionSortCellState
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

// loosely inspired from https://github.com/Zhangtaining/cell_research/blob/main/modules/Cell.py
// ...but with the actor model
// ??? is used in various places to crash if a surprise happens, but it may need to be replaced with Behaviors.same or Tinker.steadily if it's just a duplicate message and not a bug
object InsertionSortCell {

  type InsertionSortCellWrapper = CellWrapper[Message]

  // inbox 📥

  sealed trait Message

  final case class Initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]) extends Message

  case object DoSort extends Message

  // three parts: Begin (left->right), Complete (left<-right), Notify out to non-participants of the swap
  sealed trait SwapProtocol extends Message {
    def originator: Int
    def forClockTick: Int
  }

  case class BeginSwap(newLeft: Option[InsertionSortCellWrapper], originator: Int, forClockTick: Int) extends SwapProtocol

  case class CompleteSwap(newRightOrReject: Either[NoOp.type, Option[InsertionSortCellWrapper]], originator: Int, forClockTick: Int) extends SwapProtocol

  // for the non-participants to the swap
  case class NotifyOfSwap(
                           latestIndex: Int,
                           cell: Either[InsertionSortCellWrapper, InsertionSortCellWrapper],
                           //                           replacementLeftOrRight: Either[Option[InsertionSortCellWrapper], Option[InsertionSortCellWrapper]],
                           originator: Int, forClockTick: Int
                         ) extends SwapProtocol

  case class ClockTick(count: Int) extends Message

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "📵") { (context, noteRef) =>
    implicit val self: InsertionSortCellWrapper = CellWrapper(id, value, noteName, context.self)
    implicit val nr: NoteRef = noteRef
    implicit val tc: TinkerContext[?] = context
    implicit val probe: SpiritRef[Probe.Message] = Tinker.userExtension.probe
    Tinker.userExtension.probe !! Probe.Register(id, value, noteName)

    Tinker.userExtension.probe !! UpdatedState(id, InsertionSortCellState(index, None, None))

    initializing(index)
  }

  private def initializing(index: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val state = InsertionSortCellState(index, None, None)
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    nr.updateDocument("initializing", state, "initializing", retainHistory = false)

    Tinker.receiveMessage {
      case Initialize(leftNeighbor, rightNeighbor) =>
        inactive(index, leftNeighbor, rightNeighbor, -1)

      case unexpected => throw InvariantViolation(s"Unexpected message during initialization: $unexpected")
    }
  }

  // initialized with neighbors, but is waiting to sort
  private def inactive(index: Int, maybeLeftNeighbor: Option[InsertionSortCellWrapper], maybeRightNeighbor: Option[InsertionSortCellWrapper], latestClockTick: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val state = InsertionSortCellState(index, maybeLeftNeighbor, maybeRightNeighbor)
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    nr.updateDocument("inactive", state, "initialized")

    Tinker.receiveMessage {
      case DoSort => active(index, maybeLeftNeighbor, maybeRightNeighbor, latestClockTick)
      case protocol: SwapProtocol =>
        protocol match {
          case bs@BeginSwap(newLeft, _, _) =>
            // SELF is moving left because LeftNeighbor wants to move right
            maybeLeftNeighbor match {
              case Some(leftNeighbor) =>
                leftNeighbor !~! CompleteSwap(Right(maybeRightNeighbor), self.id, latestClockTick)
                // our left neighbor is moving right, and taking our index as ours is decremented to move left
                maybeRightNeighbor.foreach(_ !~! NotifyOfSwap(index, Left(leftNeighbor), self.id, latestClockTick))
                active(index - 1, newLeft, Some(leftNeighbor), latestClockTick)
              case None =>
                throw InvariantViolation(s"Received a BeginSwap message, but no left neighbor? $bs")
            }

          case NotifyOfSwap(neighborIndex, neighborCell, _, _) =>
            (neighborCell, maybeLeftNeighbor) match {
              case (Left(newLeftNeighbor), Some(leftNeighbor)) if neighborIndex == index - 1 =>
                inactive(index, Some(newLeftNeighbor), maybeRightNeighbor, latestClockTick)
              case _ => ???
            }

          case cs@CompleteSwap(_, _, _) =>
            throw InvariantViolation(s"did not expect $cs during inactive state (this should only have come back if THIS CELL sent a begin, which hasn't, right?)")
        }

      case ClockTick(count) =>
        inactive(index, maybeLeftNeighbor, maybeRightNeighbor, count)

      case init@Initialize(_, _) => throw InvariantViolation(s"init happened twice? init=$init, leftNeighbor=$maybeLeftNeighbor, rightNeighbor=$maybeRightNeighbor")
    }
  }

  // actively sorting (gated by the clock tick)
  private def active(index: Int, maybeLeftNeighbor: Option[InsertionSortCellWrapper], maybeRightNeighbor: Option[InsertionSortCellWrapper], latestClockTick: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val maybeNeighborToSwapRightWith = maybeRightNeighbor.filter(_.value < self.value)
    if (maybeNeighborToSwapRightWith.isEmpty) {
      maybeRightNeighbor.foreach(_ !~! DoSort)
    }

    val state = InsertionSortCellState(index, maybeLeftNeighbor, maybeRightNeighbor)
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    nr.updateDocument("active", state, s"""maybeNeighborToSwapRightWith=${maybeNeighborToSwapRightWith.map("[[" + _.noteName + "]]").getOrElse("None")}""")

    Tinker.receiveMessage {
      case DoSort => Tinker.steadily
      case protocol: SwapProtocol =>
        if (protocol.forClockTick < latestClockTick) ???
        protocol match {
          case BeginSwap(newLeft, _, _) =>
            // left neighbor prompted SELF to move left (swap with it)
            maybeLeftNeighbor match {
              case None => ???
              case Some(leftNeighbor) =>
                leftNeighbor !~! CompleteSwap(Right(maybeRightNeighbor), self.id, latestClockTick)
                maybeRightNeighbor.foreach(_ !~! NotifyOfSwap(index, Left(leftNeighbor), leftNeighbor.id, latestClockTick))
                newLeft.foreach(_ !~! NotifyOfSwap(index, Right(self), self.id, latestClockTick))
                active(index - 1, newLeft, Some(leftNeighbor), latestClockTick)
            }
          case NotifyOfSwap(neighborIndex, neighborCell, _, _) =>
            neighborCell match {
              case Left(newLeftNeighbor) if neighborIndex == index - 1 =>
                if (maybeLeftNeighbor.isEmpty) ??? // shouldn't happen?
                inactive(index, Some(newLeftNeighbor), maybeRightNeighbor, latestClockTick)
              case Right(newRightNeighbor) if neighborIndex == index + 1 =>
                if (maybeRightNeighbor.isEmpty) ??? // shouldn't happen?
                inactive(index, maybeLeftNeighbor, Some(newRightNeighbor), latestClockTick)

              case other =>
                val t: Int = 1
                val msg = s"(selfindex=$index) Expected left(index-1) or right(index+1) but had " +
                  s"${neighborCell.getClass.getCanonicalName}(index${neighborIndex-index}) neighborIndex=$neighborIndex ($other)"
                throw InvariantViolation(msg)
            }

          case cs@CompleteSwap(_, _, _) =>
            throw InvariantViolation(s"did not expect $cs during inactive state (this should only have come back if THIS CELL sent a begin, which hasn't, right?)")
        }

      case ClockTick(count) =>
        maybeNeighborToSwapRightWith match {
          case Some(rightNeighborToSwapWith) =>
            implicit val tc: TinkerContext[?] = context
            rightNeighborToSwapWith !~! BeginSwap(maybeLeftNeighbor, self.id, count)
            midswap(index, maybeLeftNeighbor, rightNeighborToSwapWith, count)
          case None =>
            // FIXME: log
            Tinker.steadily
        }

      case init@Initialize(_, _) => throw InvariantViolation(s"init happened twice? init=$init, leftNeighbor=$maybeLeftNeighbor, rightNeighbor=$maybeRightNeighbor")
    }
  }

  private def midswap(index: Int, maybeLeftNeighbor: Option[InsertionSortCellWrapper], oldRightNeighbor: InsertionSortCellWrapper, latestClockTick: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val state = InsertionSortCellState(index, maybeLeftNeighbor, Some(oldRightNeighbor))
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    nr.updateDocument("midswap", state, s"swapping with [[${oldRightNeighbor.noteName}]]")

    // entered this state because THIS sent BeginSwap to oldRightNeighbor, and is expecting CompleteSwap back

    Tinker.receiveMessage {
      case swap: SwapProtocol =>
        if (swap.forClockTick < latestClockTick) ???
        swap match {
          case bs@BeginSwap(_, _, _) =>
            val msg = s"already swapping, but received $bs"
            Tinker.userExtension.probe !! Probe.FoundABug(msg)
            throw InvariantViolation(msg)
          case CompleteSwap(newRightOrReject, _, _) =>
            newRightOrReject match {
              case Left(NoOp) => throw InvariantViolation("swap rejection not expected - needs implementation!")
              case Right(newRightNeighbor) =>
                // this means SELF's right neighbor, now left neighbor, has moved right while SELF moved left
                val newIndex = index + 1
                oldRightNeighbor !~! NotifyOfSwap(newIndex, Left(self), self.id, latestClockTick)
                maybeLeftNeighbor.foreach(_ !~! NotifyOfSwap(newIndex, Right(oldRightNeighbor), self.id, latestClockTick))
                active(newIndex, Some(oldRightNeighbor), newRightNeighbor, latestClockTick)
            }

          case nos@NotifyOfSwap(neighborIndex, neighborCell, _, senderClockTick) =>
            if (senderClockTick < latestClockTick) ???

            neighborCell match {
              case Left(newLeftNeighbor) if neighborIndex == index - 1 =>
                if (maybeLeftNeighbor.isEmpty) ??? // shouldn't happen?
                oldRightNeighbor !~! nos // forward it!
                active(index, Some(newLeftNeighbor), Some(oldRightNeighbor), latestClockTick)
              case Right(newRightNeighbor) if neighborIndex == index + 1 =>
                active(index, maybeLeftNeighbor, Some(newRightNeighbor), latestClockTick)

              case _ => ???
            }
        }

      case ClockTick(_) => ???
      case init@Initialize(_, _) => throw InvariantViolation(s"init happened twice? init=$init, leftNeighbor=$maybeLeftNeighbor, rightNeighbor=$oldRightNeighbor")
      case DoSort => ???
    }
  }
}
