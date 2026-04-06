package me.micseydel.app.selfsortingarrays.cell

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.cell.atom.Helper.InvariantViolation
import me.micseydel.app.selfsortingarrays.Probe
import me.micseydel.app.selfsortingarrays.Probe.UpdatedState
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.cell.atom.Helper.InsertionSortCellRichNoteRef
import me.micseydel.app.selfsortingarrays.cell.atom.{CellHistoryNote, InsertionSortCellState}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef


// loosely inspired from https://github.com/Zhangtaining/cell_research/blob/main/modules/Cell.py
// ...but with the actor model
// ??? is used in various places to crash if a surprise happens, but it may need to be replaced with Behaviors.same or Tinker.steadily if it's just a duplicate message and not a bug
object BubbleSortCell {

  type BubbleSortCellWrapper = CellWrapper[Message]

  // inbox 📥

  sealed trait Message

  final case class Initialize(leftNeighbor: Option[BubbleSortCellWrapper], rightNeighbor: Option[BubbleSortCellWrapper]) extends Message

  case object DoSort extends Message

  // three parts: Begin (left->right), Complete (left<-right), Notify out to non-participants of the swap
  sealed trait SwapProtocol extends Message {
    def originator: Int
    def forClockTick: Int
    def pretty: String
  }

  case class BeginSwap(newLeft: Option[BubbleSortCellWrapper], originator: Int, forClockTick: Int) extends SwapProtocol {
    override def pretty: String = s"BeginSwap(${newLeft.map(_.wikilink)}, $originator, $forClockTick)"
  }

  case class CompleteSwap(newRightOrReject: Either[NoOp.type, Option[BubbleSortCellWrapper]], originator: Int, forClockTick: Int) extends SwapProtocol {
    def pretty: String = s"CompleteSwap(${newRightOrReject.map(_.map(_.wikilink))}, $originator, $forClockTick)"
  }

  // for the non-participants to the swap
  case class NotifyOfSwap(
                           latestIndex: Int,
                           cell: Either[BubbleSortCellWrapper, BubbleSortCellWrapper],
                           originator: Int,
                           forClockTick: Int
                         ) extends SwapProtocol {
    def pretty: String = s"NotifyOfSwap($latestIndex, ${cell.fold("L" + _.wikilink, "R" + _.wikilink)}, $originator, $forClockTick)"
  }

  case class ClockTick(count: Int) extends Message

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "📵") { (context, noteRef) =>
    implicit val self: BubbleSortCellWrapper = CellWrapper(id, value, noteName, context.self)
    implicit val nr: NoteRef = noteRef
    implicit val tc: TinkerContext[?] = context
    implicit val probe: SpiritRef[Probe.Message] = Tinker.userExtension.probe
    Tinker.userExtension.probe !! Probe.RegisterCell(id, value, noteName)

    Tinker.userExtension.probe !! UpdatedState(id, InsertionSortCellState(index, None, None))

    initializing(index)
  }

  private def initializing(index: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: BubbleSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val state = InsertionSortCellState(index, None, None)
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    implicit val historyHolder: SpiritRef[CellHistoryNote.Message] = context.cast(CellHistoryNote(self.id), "CellHistoryNote")
    nr.updateDocument("initializing", state, "- initializing")

    Tinker.receiveMessage {
      case Initialize(leftNeighbor, rightNeighbor) =>
        inactive(index, leftNeighbor, rightNeighbor, -1, "- initialized")

      case unexpected => InvariantViolation(s"Unexpected message during initialization: $unexpected", state)
    }
  }

  // initialized with neighbors, but is waiting to sort
  private def inactive(index: Int, maybeLeftNeighbor: Option[BubbleSortCellWrapper], maybeRightNeighbor: Option[BubbleSortCellWrapper], latestClockTick: Int, summaryOfPriorSelflet: String)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: BubbleSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message], historyHolder: SpiritRef[CellHistoryNote.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val state = InsertionSortCellState(index, maybeLeftNeighbor, maybeRightNeighbor)
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    nr.updateDocument("inactive", state, summaryOfPriorSelflet)

    Tinker.receiveMessage {
      case DoSort => active(index, maybeLeftNeighbor, maybeRightNeighbor, latestClockTick, "- Ignored DoSort")
      case protocol: SwapProtocol =>
        protocol match {
          case bs@BeginSwap(newLeft, originator, _) =>
            // SELF is moving left because LeftNeighbor wants to move right
            maybeLeftNeighbor match {
              case Some(leftNeighbor) =>
                // our left neighbor is moving right, and taking our index as ours is decremented to move left

                val completeSwapMessage = CompleteSwap(Right(maybeRightNeighbor), self.id, latestClockTick)
                leftNeighbor !~! completeSwapMessage

                val notifyOfSwapIfRightNeighbor = NotifyOfSwap(index, Left(leftNeighbor), self.id, latestClockTick)
                maybeRightNeighbor.foreach(_ !~! notifyOfSwapIfRightNeighbor)

                val newIndex = index - 1

                val summary = List(
                  Some(s"- $originator has initiated a swap; index decrementing to $newIndex"),
                  Some(s"    - state change: left(${leftNeighbor.wikilink}->${newLeft.map(_.wikilink).getOrElse(None.toString)}) and right(${maybeRightNeighbor.map(_.wikilink)}->${leftNeighbor.wikilink})"),
                  Some(s"    - sent messages:"),
                  Some(s"        - ${leftNeighbor.wikilink} ! ${completeSwapMessage.pretty} (telling initiator(o=$originator) to finish it)"),
                  maybeRightNeighbor.map(rightNeighbor => s"        - ${rightNeighbor.wikilink} ! ${notifyOfSwapIfRightNeighbor.pretty}")
                ).flatten.mkString("\n")

                active(newIndex, newLeft, Some(leftNeighbor), latestClockTick, summary)
              case None =>
                InvariantViolation(s"Received a BeginSwap message, but no left neighbor? $bs", state)
            }

          case NotifyOfSwap(neighborIndex, neighborCell, _, _) =>
            (neighborCell, maybeLeftNeighbor) match {
              case (Left(newLeftNeighbor), Some(leftNeighbor)) if neighborIndex == index - 1 =>
                inactive(index, Some(newLeftNeighbor), maybeRightNeighbor, latestClockTick, s"- notified of swap by left neighbor [[${leftNeighbor.noteName}]]->[[${newLeftNeighbor.noteName}]] (maintaining right neighbor ${maybeRightNeighbor.map(_.noteName).map("[[" + _ + "]]")})")
              case _ => ???
            }

          case cs@CompleteSwap(_, _, _) =>
            InvariantViolation(s"did not expect $cs during inactive state (this should only have come back if THIS CELL sent a begin, which hasn't, right?)", state)
        }

      case ct@ClockTick(count) =>
        inactive(index, maybeLeftNeighbor, maybeRightNeighbor, count, s"- ignored $ct")

      case init@Initialize(_, _) => InvariantViolation(s"init happened twice? init=$init, leftNeighbor=$maybeLeftNeighbor, rightNeighbor=$maybeRightNeighbor", state)
    }
  }

  // actively sorting (gated by the clock tick)
  private def active(index: Int, maybeLeftNeighbor: Option[BubbleSortCellWrapper], maybeRightNeighbor: Option[BubbleSortCellWrapper], latestClockTick: Int, summaryOfPriorSelflet: String)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: BubbleSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message], historyHolder: SpiritRef[CellHistoryNote.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val maybeNeighborToSwapRightWith = maybeRightNeighbor.filter(_.value < self.value)
    if (maybeNeighborToSwapRightWith.isEmpty) {
      maybeRightNeighbor.foreach(_ !~! DoSort)
    }

    val state = InsertionSortCellState(index, maybeLeftNeighbor, maybeRightNeighbor)
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    nr.updateDocument("active", state,summaryOfPriorSelflet)

    Tinker.receiveMessage {
      case DoSort => Tinker.steadily
      case protocol: SwapProtocol =>
        if (protocol.forClockTick < latestClockTick) ???
        protocol match {
          case BeginSwap(newLeft, originator, _) =>
            // left neighbor prompted SELF to move left (swap with it)
            maybeLeftNeighbor match {
              case None => ???
              case Some(leftNeighbor) =>
                val completeSwapMessage = CompleteSwap(Right(maybeRightNeighbor), self.id, latestClockTick)
                leftNeighbor !~! completeSwapMessage

                val notifyOfSwapRightMessage = NotifyOfSwap(index, Left(leftNeighbor), leftNeighbor.id, latestClockTick)
                maybeRightNeighbor.foreach(_ !~! notifyOfSwapRightMessage)

                val newIndex = index - 1

                val notifyOfSwapLeftMessage = NotifyOfSwap(newIndex, Right(self), self.id, latestClockTick)
                newLeft.foreach(_ !~! notifyOfSwapLeftMessage)

                val msgs: List[String] = List(
                  Some(s"        - [[${leftNeighbor.noteName}]] ! ${completeSwapMessage.pretty} (told old left neighbor to complete the swap with its new right neighbor from my old right neighbor)"),
                  maybeRightNeighbor.map("        - [[" + _.noteName + s"]] ! ${notifyOfSwapRightMessage.pretty} (told my old right neighbor its new left is my old left)"),
                  newLeft.map("        - [[" + _.noteName + s"]] ! ${notifyOfSwapLeftMessage.pretty} (told my new left that I'm its new right)")
                ).flatten
                val summary = (
                  s"- $originator triggered swap" ::
                    s"    - decremented index to $newIndex" ::
                    s"    - State change: left(${leftNeighbor.wikilink}->${newLeft.map(_.wikilink)}) and right(${maybeRightNeighbor.map(_.wikilink)}->${leftNeighbor.wikilink})" ::
                    s"    - sent ${msgs.size} msgs:" ::
                    msgs
                ).mkString("\n")
                active(newIndex, newLeft, Some(leftNeighbor), latestClockTick, summary)
            }
          case nos@NotifyOfSwap(neighborIndex, neighborCell, originator, _) =>
            neighborCell match {
              case Left(newLeftNeighbor) if neighborIndex == index - 1 =>
                if (maybeLeftNeighbor.isEmpty) ??? // shouldn't happen?
                active(index, Some(newLeftNeighbor), maybeRightNeighbor, latestClockTick, s"- notified of left swap [[${maybeLeftNeighbor.get.noteName}]]->[[${newLeftNeighbor.noteName}]]")
              case Right(newRightNeighbor) if neighborIndex == index + 1 =>
                if (maybeRightNeighbor.isEmpty) ??? // shouldn't happen?
                active(index, maybeLeftNeighbor, Some(newRightNeighbor), latestClockTick, s"- notified of right swap [[${maybeRightNeighbor.get.noteName}]]->[[${newRightNeighbor.noteName}]]")

              case Right(_) if neighborIndex == index + 2 =>
                if (maybeRightNeighbor.isEmpty) ??? // shouldn't happen?
                maybeRightNeighbor.foreach(_ !~! nos) // forward!
                active(index, maybeLeftNeighbor, maybeRightNeighbor, latestClockTick, s"- forwarded $nos to $maybeRightNeighbor")

              case other =>
                val rawOffset = neighborIndex-index
                val offset = if (rawOffset < 0) {
                  s"- $rawOffset"
                } else {
                  s"+ $rawOffset"
                }

                val invariantViolationSummary = List(
                  s"- \\[$index] ==Notified of swap by $originator, expected {left(index-1), right(index+1)}==",
                  s"    - actual: ${neighborCell.getClass.getSimpleName}(index $offset)",
                  s"    - neighborIndex=$neighborIndex",
                  s"""    - msg: ${other.fold("L" + _.wikilink, "R" + _.wikilink)}"""
                ).mkString("\n")

                InvariantViolation(invariantViolationSummary, state)
            }

          case cs@CompleteSwap(_, _, _) =>
            InvariantViolation(s"did not expect $cs during inactive state (this should only have come back if THIS CELL sent a begin, which hasn't, right?)", state)
        }

      case ClockTick(count) =>
        maybeNeighborToSwapRightWith match {
          case Some(rightNeighborToSwapWith) =>
            implicit val tc: TinkerContext[?] = context

            val beginSwapMessage = BeginSwap(maybeLeftNeighbor, self.id, count)
            rightNeighborToSwapWith !~! beginSwapMessage

            val summary = List(
              s"- Clock ticked $count, initiating swapping with ${rightNeighborToSwapWith.wikilink} (**midswap now** for tick=$count)",
              s"    - just sent ${beginSwapMessage.pretty} to ${rightNeighborToSwapWith.wikilink}"
            ).mkString("\n")
            midswap(index, maybeLeftNeighbor, rightNeighborToSwapWith, count, summary)
          case None =>
            active(index, maybeLeftNeighbor, maybeRightNeighbor, count, s"""- clock ticked ($count) but not swapping with right (${maybeRightNeighbor.map(_.wikilink).getOrElse("None")})""")
        }

      case init@Initialize(_, _) => InvariantViolation(s"init happened twice? init=$init, leftNeighbor=$maybeLeftNeighbor, rightNeighbor=$maybeRightNeighbor", state)
    }
  }

  private def midswap(index: Int, maybeLeftNeighbor: Option[BubbleSortCellWrapper], oldRightNeighbor: BubbleSortCellWrapper, latestClockTick: Int, previously: String)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: BubbleSortCellWrapper, nr: NoteRef, probe: SpiritRef[Probe.Message], historyHolder: SpiritRef[CellHistoryNote.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    val state = InsertionSortCellState(index, maybeLeftNeighbor, Some(oldRightNeighbor))
    Tinker.userExtension.probe !! UpdatedState(self.id, state)
    nr.updateDocument("midswap", state, previously)

    // entered this state because THIS sent BeginSwap to oldRightNeighbor, and is expecting CompleteSwap back

    Tinker.receiveMessage {
      case swap: SwapProtocol =>
        if (swap.forClockTick < latestClockTick) {
          InvariantViolation(s"- ==latestClockTick=$latestClockTick but received swap for ${swap.forClockTick}: ${swap.pretty}==", state)
        } else {
          swap match {
            case bs@BeginSwap(_, _, _) =>
              val msg = s"already swapping, but received $bs"
              Tinker.userExtension.probe !! Probe.FoundABug(msg)
              InvariantViolation(msg, state)
            case CompleteSwap(newRightOrReject, _, _) =>
              newRightOrReject match {
                case Left(NoOp) => InvariantViolation("swap rejection not expected - needs implementation!", state)
                case Right(newRightNeighbor) =>
                  // this moved right, so...
                  val newIndex = index + 1

                  // old right neighbor has our old index
                  val notifyOfSwapToLeftNeighbor = NotifyOfSwap(index, Right(oldRightNeighbor), self.id, latestClockTick)
                  maybeLeftNeighbor.foreach(_ !~! notifyOfSwapToLeftNeighbor)

                  val summary = List(
                    Some(s"- completed swap, moved right->"),
                    Some(s"    - incremented index to $newIndex"),
                    Some(s"    - state change: left(${maybeLeftNeighbor.map(_.wikilink)}->${oldRightNeighbor.wikilink}) and right(${oldRightNeighbor.wikilink}->${newRightNeighbor.map(_.wikilink)})"),
                    Some("    - sent messages"),
                    maybeLeftNeighbor.map(leftNeighbor => s"        - [[${leftNeighbor.noteName}]] ! ${notifyOfSwapToLeftNeighbor.pretty} (telling our old left neighbor its new right is the old right that this swapped with)")
                  ).flatten.mkString("\n")

                  active(newIndex, Some(oldRightNeighbor), newRightNeighbor, latestClockTick, summary)
              }

            case nos@NotifyOfSwap(neighborIndex, neighborCell, _, senderClockTick) =>
              if (senderClockTick < latestClockTick) ???

              neighborCell match {
                case Left(newLeftNeighbor) if neighborIndex == index - 1 =>
                  if (maybeLeftNeighbor.isEmpty) ??? // shouldn't happen?
                  oldRightNeighbor !~! nos // forward it!
                  midswap(index, Some(newLeftNeighbor), oldRightNeighbor, latestClockTick, s"- notified of left swap, ==**forwarded**== to [[${oldRightNeighbor.noteName}]] (staying in midswap)")
                case Right(newRightNeighbor) if neighborIndex == index + 1 =>
                  midswap(index, maybeLeftNeighbor, newRightNeighbor, latestClockTick, s"- notified of right swap, [[${oldRightNeighbor.noteName}]]->[[${newRightNeighbor.noteName}]] (staying in midswap)")

                case _ => ???
              }
          }
        }

      case ClockTick(count) => InvariantViolation(s"should have left midswap before clock tick $count", state)
      case init@Initialize(_, _) => InvariantViolation(s"init happened twice? init=$init, leftNeighbor=$maybeLeftNeighbor, rightNeighbor=$oldRightNeighbor", state)

      case DoSort =>
        Tinker.steadily
    }
  }
}
