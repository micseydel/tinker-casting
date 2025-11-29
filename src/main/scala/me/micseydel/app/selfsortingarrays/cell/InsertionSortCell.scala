package me.micseydel.app.selfsortingarrays.cell

import cats.data.NonEmptyList
import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.{Probe, SelfSortingArrays}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import scala.annotation.tailrec
import scala.util.{Failure, Success}

// loosely inspired from https://github.com/Zhangtaining/cell_research/blob/main/modules/Cell.py
// ...but with the actor model
// ??? is used in various places to crash if a surprise happens, but it may need to be replaced with Behaviors.same or Tinker.steadily if it's just a duplicate message and not a bug
object InsertionSortCell {

  // use this to allow Obsidian to create an animation
  private val SleepDurationMillis = 1000

  type InsertionSortCellWrapper = CellWrapper[Message]

  // inbox 📥

  sealed trait Message

  final case class Initialize(leftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: Option[InsertionSortCellWrapper]) extends Message

  sealed trait MessageToRespondTo extends Message {
    def replyTo: SpiritRef[NonEmptyList[InsertionSortCellWrapper]]
  }

  final case class GetDownstreamListContents(replyTo: SpiritRef[NonEmptyList[InsertionSortCellWrapper]]) extends MessageToRespondTo

  final case class RequestSortedContents(replyTo: SpiritRef[NonEmptyList[InsertionSortCellWrapper]]) extends MessageToRespondTo

  private sealed trait NeighborChange extends Message

  private case class NewRight(newRight: Option[InsertionSortCellWrapper]) extends NeighborChange

  private case class NewLeft(newLeft: InsertionSortCellWrapper) extends NeighborChange

  private case class NewReplyTo(newReplyTo: SpiritRef[NonEmptyList[InsertionSortCellWrapper]]) extends NeighborChange

  private sealed trait ReceiveAggregation extends Message {
    def downstream: NonEmptyList[InsertionSortCellWrapper]
  }

  private case class ReceiveDownstream(downstream: NonEmptyList[InsertionSortCellWrapper]) extends ReceiveAggregation

  private case class ReceiveSortedDownstream(downstream: NonEmptyList[InsertionSortCellWrapper]) extends ReceiveAggregation

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = {
    initializing(id, index, noteName, value)
  }

  private def initializing(id: Int, index: Int, noteName: String, value: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "📵") { (context, noteRef) =>
    implicit val self: InsertionSortCellWrapper = CellWrapper(id, value, noteName, context.self)
    implicit val nr: NoteRef = noteRef
    Tinker.receiveMessage {
      case Initialize(leftNeighbor, maybeRightNeighbor) =>
        maybeRightNeighbor match {
          case Some(rightNeighbor) =>
            waiting(InsertionCellStateForWaiting(index, leftNeighbor, rightNeighbor))
          case None =>
            sorted(InsertionCellStateForSorted(index, leftNeighbor, None, Nil))
        }

      case _ => ???
    }
  }

  private def waiting(state: InsertionCellStateForWaiting)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef): Ability[Message] = {
    if (state.maybeLeftNeighbor.contains(self)) {
      throw new RuntimeException(s"${state.maybeLeftNeighbor} contains self!")
    }
    if (state.rightNeighbor == self) {
      throw new RuntimeException(s"right neighbor is self!")
    }

    Tinker.setup { context =>
      implicit val tc: TinkerContext[?] = context
      Tinker.userExtension.probe !! state.probe

      noteRef.setRaw(
        s"""---
           |tags: [waiting]
           |---
           |- start index: ${state.index}
           |
           |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| [[${state.rightNeighbor.noteName}]] ->
           |""".stripMargin) match {
        case Failure(exception) => throw exception
        case Success(NoOp) =>
      }

      Tinker.receiveMessage {
        case GetDownstreamListContents(replyTo) =>
          state.rightNeighbor !! GetDownstreamListContents(context.messageAdapter(ReceiveDownstream))
          awaitingAggregation(InsertionCellStateForAwaitingAsHead(state.index, replyTo, state.maybeLeftNeighbor, state.rightNeighbor))

        case RequestSortedContents(replyTo) =>
          state.rightNeighbor !! RequestSortedContents(context.messageAdapter(ReceiveSortedDownstream))
          awaitingSortedDownstream(InsertionCellStateForAwaitingAsHead(state.index, replyTo, state.maybeLeftNeighbor, state.rightNeighbor))

        case ReceiveSortedDownstream(sortedDownstream) =>
          sorted(InsertionCellStateForSorted(state.index, state.maybeLeftNeighbor, Some(sortedDownstream.head), sortedDownstream.toList))

        case _: Initialize | _: NeighborChange | _: ReceiveDownstream => ???
      }
    }
  }

  private def awaitingAggregation(state: InsertionCellStateForAwaiting)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.userExtension.probe !! state.probe

    val label: String = state match {
      case InsertionCellStateForAwaitingAsHead(_, replyTo, _, _) => s"$replyTo"
      case InsertionCellStateForAwaitingAsNONHead(_, _, _) => "(left)"
    }

    noteRef.setRaw(
      s"""---
         |tags: [awaitingAggregation]
         |---
         |- start index: ${state.index}
         |- replyTo: $label
         |
         |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| [[${state.rightNeighbor.noteName}]] ->
         |""".stripMargin) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }
    Tinker.receiveMessage {
      case aggregation: ReceiveAggregation =>
        state match {
          case InsertionCellStateForAwaitingAsHead(_, replyTo, _, _) =>
            replyTo !! aggregation.downstream.prepend(self)
          case InsertionCellStateForAwaitingAsNONHead(_, leftNeighbor, _) =>
            leftNeighbor !! ReceiveSortedDownstream(aggregation.downstream.prepend(self))
        }
        waiting(InsertionCellStateForWaiting(state.index, state.maybeLeftNeighbor, aggregation.downstream.head))

      case _: GetDownstreamListContents | _: RequestSortedContents | _: NeighborChange | _: Initialize => ???
    }
  }

  private def awaitingSortedDownstream(state: InsertionCellStateForAwaiting)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.userExtension.probe !! state.probe

    val secondLine = state match {
      case InsertionCellStateForAwaitingAsHead(_, replyTo, _, _) => s"replyTo: $replyTo"
      case InsertionCellStateForAwaitingAsNONHead(_, leftNeighbor, _) => s"leftNeighbor: $leftNeighbor"
    }

    noteRef.setRaw(
      s"""---
         |tags: [awaitingSortedDownstream]
         |---
         |- start index: ${state.index}
         |- $secondLine
         |- state type ${state.getClass}
         |
         |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| [[${state.rightNeighbor.noteName}]] ->
         |""".stripMargin) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    Tinker.receiveMessage {
      case ReceiveSortedDownstream(downstream) =>
        Thread.sleep(SleepDurationMillis) // FIXME: gotta be a better way...

        if (self.value > downstream.head.value) {
          findNewDownstreamNeighbors(self.value, downstream) match {
            case (newLeft, updatedDownstream) =>
              val updateForRightNeighbor = state match {
                case InsertionCellStateForAwaitingAsHead(_, _, Some(leftNeighbor), _) =>
                  NewLeft(leftNeighbor)
                case InsertionCellStateForAwaitingAsHead(_, replyTo, None, _) =>
                  NewReplyTo(replyTo)
                case InsertionCellStateForAwaitingAsNONHead(_, leftNeighbor, _) =>
                  NewLeft(leftNeighbor)
              }

              // notify our old neighbors of their new neighbors...
              // FIXME: unfortunately, there's a race-condition here: if Right doesn't get this message before propagation left reaches it (unlikely), it will send its result to SELF instead of Left
              state.rightNeighbor !! updateForRightNeighbor
              state.maybeLeftNeighbor.foreach(_ !! NewRight(Some(state.rightNeighbor)))
              newLeft !! NewRight(Some(self))
              updatedDownstream.headOption.foreach(_ !! NewLeft(self))

              // now propagate the update toward the head
              newLeft !! ReceiveSortedDownstream(NonEmptyList(self, updatedDownstream))

              sorted(InsertionCellStateForSorted(state.index, Some(newLeft), updatedDownstream.headOption, updatedDownstream))
          }
        } else {
          state match {
            case InsertionCellStateForAwaitingAsHead(_, replyTo, _, _) =>
              replyTo !! downstream.prepend(self)
            case InsertionCellStateForAwaitingAsNONHead(_, leftNeighbor, _) =>
              leftNeighbor !! ReceiveSortedDownstream(downstream.prepend(self))
          }

          sorted(InsertionCellStateForSorted(state.index, state.maybeLeftNeighbor, Some(state.rightNeighbor), downstream.toList))
        }

      case newNeighbor: NeighborChange =>
        newNeighbor match {
          case NewRight(newRight) =>
            newRight match {
              case Some(rightNeighbor) =>
                awaitingSortedDownstream(state match {
                  case InsertionCellStateForAwaitingAsHead(index, replyTo, _, _) =>
                    InsertionCellStateForAwaitingAsHead(index, replyTo, state.maybeLeftNeighbor, rightNeighbor)
                  case InsertionCellStateForAwaitingAsNONHead(index, leftNeighbor, _) =>
                    InsertionCellStateForAwaitingAsNONHead(index, leftNeighbor, rightNeighbor)
                })
              case None =>
                sorted(InsertionCellStateForSorted(state.index, state.maybeLeftNeighbor, None, Nil))
            }
          case NewLeft(newLeft) =>
            awaitingSortedDownstream(InsertionCellStateForAwaitingAsNONHead(state.index, newLeft, state.rightNeighbor))
          case NewReplyTo(replyTo) =>
            awaitingSortedDownstream(InsertionCellStateForAwaitingAsHead(state.index, replyTo, None, state.rightNeighbor))
        }

      case _: Initialize | _: GetDownstreamListContents | _: RequestSortedContents | _: ReceiveDownstream => ???
    }
  }

  private def sorted(state: InsertionCellStateForSorted)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: InsertionSortCellWrapper, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.userExtension.probe !! state.probe

    noteRef.setRaw(
      s"""---
         |tags: [sorted]
         |---
         |- start index: ${self.id}
         |- sortedDownstream: ${state.sortedDownstream.map(_.value)}
         |
         |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| ${state.maybeRightNeighbor.map(_.noteName).map(s => s"[[$s]] ->").getOrElse("")}
         |""".stripMargin) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    Tinker.receiveMessage {
      case message: MessageToRespondTo =>
        message.replyTo !! NonEmptyList(self, state.sortedDownstream)
        Tinker.steadily

      case ReceiveSortedDownstream(downstream) =>
        // if there's a left neighbor, it needs the latest downstream
        for (leftNeighbor <- state.maybeLeftNeighbor) {
          leftNeighbor !! ReceiveSortedDownstream(downstream.prepend(self))
        }

        sorted(state.copy(sortedDownstream = downstream.toList))

      case newNeighbor: NeighborChange =>
        newNeighbor match {
          case NewReplyTo(replyTo) =>
            state.maybeRightNeighbor match {
              case Some(rightNeighbor) =>
                awaitingSortedDownstream(InsertionCellStateForAwaitingAsHead(state.index, replyTo, None, rightNeighbor))
              case None => sorted(InsertionCellStateForSorted(state.index, None, None, Nil))
            }

          case NewLeft(newLeftNeighbor) =>
            sorted(InsertionCellStateForSorted(state.index, Some(newLeftNeighbor), state.maybeRightNeighbor, state.sortedDownstream))

          case NewRight(Some(newRightNeighbor)) =>
            val updatedState = state.maybeLeftNeighbor match {
              case Some(leftNeighbor) => InsertionCellStateForAwaitingAsNONHead(state.index, leftNeighbor, newRightNeighbor)
              case None =>
                ??? // this shouldn't happen because if the head was sorted, that SHOULD have meant the tail was sorted!
            }
            awaitingSortedDownstream(updatedState)

          case NewRight(None) =>
            sorted(InsertionCellStateForSorted(state.index, state.maybeLeftNeighbor, None, Nil))
        }

      case _: Initialize | _: ReceiveDownstream => ???
    }
  }

  //

  // returns (newLeft, remainingSortedDownstream) where remainingSortedDownstream.headOption is the newRight
  private def findNewDownstreamNeighbors(value: Int, sortedDownstream: NonEmptyList[InsertionSortCellWrapper]): (InsertionSortCellWrapper, List[InsertionSortCellWrapper]) = {
    @tailrec
    def helper(remaining: NonEmptyList[InsertionSortCellWrapper]): (InsertionSortCellWrapper, List[InsertionSortCellWrapper]) = {
      remaining match {
        case NonEmptyList(justOne, Nil) => (justOne, Nil)
        case NonEmptyList(maybeOurNewLeft, newDownstream@maybeOurNewRight :: _) if maybeOurNewRight.value >= value =>
          (maybeOurNewLeft, newDownstream)
        case NonEmptyList(_, head :: tail) =>
          helper(NonEmptyList(head, tail))
        case _ => ???
      }
    }

    helper(sortedDownstream)
  }

  //

  private sealed trait InsertionCellState {
    def index: Int

    def probe(implicit cw: InsertionSortCellWrapper): Probe.RecordLatest
  }

  private case class InsertionCellStateForWaiting(index: Int, maybeLeftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: InsertionSortCellWrapper) extends InsertionCellState {
    override def probe(implicit cw: InsertionSortCellWrapper): Probe.RecordLatest = Probe.RecordLatest(
      id = cw.id,
      filename = SelfSortingArrays.filename(cw.id, cw.value),
      maybeLeft = maybeLeftNeighbor.map(_.noteName),
      maybeRight = Some(rightNeighbor.noteName)
    )
  }

  private case class InsertionCellStateForSorted(index: Int, maybeLeftNeighbor: Option[InsertionSortCellWrapper], maybeRightNeighbor: Option[InsertionSortCellWrapper], sortedDownstream: List[InsertionSortCellWrapper]) extends InsertionCellState {
    override def probe(implicit cw: InsertionSortCellWrapper): Probe.RecordLatest = Probe.RecordLatest(
      id = cw.id,
      filename = SelfSortingArrays.filename(cw.id, cw.value),
      maybeLeft = maybeLeftNeighbor.map(_.noteName),
      maybeRight = maybeRightNeighbor.map(_.noteName)
    )
  }

  private sealed trait InsertionCellStateForAwaiting extends InsertionCellState {
    def rightNeighbor: InsertionSortCellWrapper

    def maybeLeftNeighbor: Option[InsertionSortCellWrapper]
  }

  private case class InsertionCellStateForAwaitingAsHead(index: Int, replyTo: SpiritRef[NonEmptyList[InsertionSortCellWrapper]], maybeLeftNeighbor: Option[InsertionSortCellWrapper], rightNeighbor: InsertionSortCellWrapper) extends InsertionCellStateForAwaiting {
    override def probe(implicit cw: InsertionSortCellWrapper): Probe.RecordLatest = Probe.RecordLatest(
      id = cw.id,
      filename = SelfSortingArrays.filename(cw.id, cw.value),
      maybeLeft = None,
      maybeRight = Some(rightNeighbor.noteName)
    )
  }

  private case class InsertionCellStateForAwaitingAsNONHead(index: Int, leftNeighbor: InsertionSortCellWrapper, rightNeighbor: InsertionSortCellWrapper) extends InsertionCellStateForAwaiting {
    override def probe(implicit cw: InsertionSortCellWrapper): Probe.RecordLatest = Probe.RecordLatest(
      id = cw.id,
      filename = SelfSortingArrays.filename(cw.id, cw.value),
      maybeLeft = None,
      maybeRight = Some(rightNeighbor.noteName)
    )

    override def maybeLeftNeighbor: Option[InsertionSortCellWrapper] = Some(leftNeighbor)
  }
}
