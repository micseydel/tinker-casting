package me.micseydel.app.selfsortingarrays

import cats.data.NonEmptyList
import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

import scala.annotation.tailrec
import scala.util.{Failure, Success}

// loosely inspired from https://github.com/Zhangtaining/cell_research/blob/main/modules/Cell.py
// ...but with the actor model
// note: this is note a tight data model, and ??? is used in various places to crash if a surprise happens
object Cell {

  // inbox 📥

  sealed trait Message

  sealed trait MessageToRespondTo extends Message {
    def replyTo: SpiritRef[NonEmptyList[CellWrapper]]
  }

  final case class GetDownstreamListContents(replyTo: SpiritRef[NonEmptyList[CellWrapper]]) extends MessageToRespondTo

  final case class RequestSortedContents(replyTo: SpiritRef[NonEmptyList[CellWrapper]]) extends MessageToRespondTo

  private case class NewNeighbor(leftOrRight: Either[Option[CellWrapper], Option[CellWrapper]],
                                 retainSort: Boolean = false,
                                 maybeExternalReplyTo: Option[SpiritRef[NonEmptyList[CellWrapper]]] = None
                                ) extends Message

  private case class ReceiveSortedDownstream(sortedDownstream: NonEmptyList[CellWrapper]) extends Message

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int, remaining: List[(Int, String, Int)])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = {
    setup(id, index, noteName, value, remaining, None)
  }

  private def setup(id: Int, index: Int, noteName: String, value: Int, remaining: List[(Int, String, Int)], leftNeighbor: Option[CellWrapper])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "📵") { (context, noteRef) =>
    implicit val self: CellWrapper = CellWrapper(id, value, noteName, context.self)
    implicit val nr: NoteRef = noteRef
    remaining match {
      case Nil =>
        waiting(CellState(id, index, value, leftNeighbor, None))
      case (nextId, nextNoteName, nextValue) :: tail =>
        val rightNeighbor = context.cast(Cell.setup(nextId, index+1, nextNoteName, nextValue, tail, leftNeighbor = Some(self)), nextNoteName.replace(" ", "_").replace("(", "").replace(")", ""))
        waiting(CellState(id, index, value, leftNeighbor, Some(CellWrapper(nextId, nextValue, nextNoteName, rightNeighbor))))
    }
  }

  private def waiting(state: CellState)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: CellWrapper, noteRef: NoteRef): Ability[Message] = {
    if (state.maybeLeftNeighbor.contains(self)) {
      throw new RuntimeException(s"${state.maybeLeftNeighbor} contains self!")
    }
    if (state.maybeRightNeighbor.contains(self)) {
      throw new RuntimeException(s"${state.maybeRightNeighbor} contains self!")
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
           |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| ${state.maybeRightNeighbor.map(_.noteName).map(s => s"[[$s]] ->").getOrElse("")}
           |""".stripMargin) match {
        case Failure(exception) => throw exception
        case Success(NoOp) =>
      }

      Tinker.receiveMessage {
        case GetDownstreamListContents(replyTo) =>
          state.maybeRightNeighbor match {
            case None =>
              replyTo !! NonEmptyList.of(self)
              Tinker.steadily
            case Some(rightNeighbor) =>
              rightNeighbor !! GetDownstreamListContents(context.messageAdapter(ReceiveSortedDownstream))
              awaitingAggregation(state, replyTo)
          }

        case RequestSortedContents(replyTo) =>
          state.maybeRightNeighbor match {
            case None =>
              replyTo !! NonEmptyList.of(self)
              sorted(state, Nil)
            case Some(rightNeighbor) =>
              rightNeighbor !! RequestSortedContents(context.messageAdapter(ReceiveSortedDownstream))
              awaitingSortedDownstream(state, Some(replyTo))
          }

        case ReceiveSortedDownstream(sortedDownstream) =>
          sorted(state.withUpdatedRight(Some(sortedDownstream.head)), sortedDownstream.toList)

        case NewNeighbor(_, _, _) => ???
      }
    }
  }

  private def awaitingAggregation(state: CellState, replyTo: SpiritRef[NonEmptyList[CellWrapper]])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: CellWrapper, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.userExtension.probe !! state.probe
    noteRef.setRaw(
      s"""---
         |tags: [awaitingAggregation]
         |---
         |- start index: ${state.index}
         |- replyTo: $replyTo
         |
         |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| ${state.maybeRightNeighbor.map(_.noteName).map(s => s"[[$s]] ->").getOrElse("")}
         |""".stripMargin) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }
    Tinker.receiveMessage {
      case ReceiveSortedDownstream(downstream) =>
        replyTo !! downstream.prepend(self)
        waiting(state)

      case GetDownstreamListContents(_) => ???
      case RequestSortedContents(_) => ???
      case NewNeighbor(_, _, _) => ???
    }
  }

  private def awaitingSortedDownstream(state: CellState, maybeCallerReplyTo: Option[SpiritRef[NonEmptyList[CellWrapper]]])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: CellWrapper, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.userExtension.probe !! state.probe

    noteRef.setRaw(
      s"""---
         |tags: [awaitingSortedDownstream]
         |---
         |- start index: ${state.index}
         |- maybeCallerReplyTo: $maybeCallerReplyTo
         |
         |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| ${state.maybeRightNeighbor.map(_.noteName).map(s => s"[[$s]] ->").getOrElse("")}
         |""".stripMargin) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    Tinker.receiveMessage {
      case ReceiveSortedDownstream(downstream) =>
        Thread.sleep(1000) // FIXME: BAD BAD
        val justNums = downstream.map(_.value).toList
        if (justNums != justNums.sorted) {
          println(s"[awaitingSortedDownstream ${state.value}] downstream not sorted: $justNums")
          ???
        }

        if (state.value > downstream.head.value) {
          findNewDownstreamNeighbors(state.value, downstream) match {
            case (newLeft, updatedDownstream) =>
              // notify our old neighbors of their new neighbors
              // FIXME: unfortunately, there's a race-condition here: if Right doesn't get this message before propagation left reaches it (unlikely), it will send its result to SELF instead of Left
              state.maybeRightNeighbor.foreach(_ !! NewNeighbor(Left(state.maybeLeftNeighbor), maybeExternalReplyTo = maybeCallerReplyTo))
              state.maybeLeftNeighbor.foreach(_ !! NewNeighbor(Right(Some(state.maybeRightNeighbor.get)))) // FIXME: sloppy .get, but receiving this message means we should have a right neighbor so this creates a thrown exception if we need it
              // notify our new neighbors - right is special, it will remain sorted
              newLeft !! NewNeighbor(Right(Some(self)))
              updatedDownstream.headOption.foreach(_ !! NewNeighbor(Left(Some(self)), retainSort = true))

              // now propagate the update toward the head
              newLeft !! ReceiveSortedDownstream(NonEmptyList(self, updatedDownstream))

              sorted(
                state
                  .withUpdatedLeft(Some(newLeft))
                  .withUpdatedRight(updatedDownstream.headOption),
                updatedDownstream
              )
          }
        } else {
          maybeCallerReplyTo match {
            case Some(replyTo) =>
              replyTo !! downstream.prepend(self)
            case None =>
              state.maybeLeftNeighbor match {
                case None => ???
                case Some(replyTo) =>
                  replyTo !! ReceiveSortedDownstream(downstream.prepend(self))
              }
          }

          sorted(state, downstream.toList)
        }

      case NewNeighbor(leftOrRight, retainSort, maybeExternalReplyTo) =>
        if (retainSort) ???
        val updatedState = leftOrRight match {
          case Left(newLeft) => state.withUpdatedLeft(newLeft)
          case Right(newRight) => state.withUpdatedRight(newRight)
        }
        if (maybeExternalReplyTo.nonEmpty) {
          if (maybeCallerReplyTo.nonEmpty) ???
          awaitingSortedDownstream(updatedState, maybeExternalReplyTo)
        } else {
          awaitingSortedDownstream(updatedState, maybeCallerReplyTo)
        }

      case GetDownstreamListContents(_) => ???
      case RequestSortedContents(_) => ???
    }
  }

  private def sorted(state: CellState, sortedDownstream: List[CellWrapper])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], self: CellWrapper, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.userExtension.probe !! state.probe

    noteRef.setRaw(
      s"""---
         |tags: [sorted]
         |---
         |- start index: ${state.index}
         |- sortedDownstream: ${sortedDownstream.map(_.value)}
         |
         |${state.maybeLeftNeighbor.map(_.noteName).map(s => s"<- [[$s]] ").getOrElse("")}| ${state.maybeRightNeighbor.map(_.noteName).map(s => s"[[$s]] ->").getOrElse("")}
         |""".stripMargin) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    Tinker.receiveMessage {
      case message: MessageToRespondTo =>
        message.replyTo !! NonEmptyList(self, sortedDownstream)
        Tinker.steadily

      case ReceiveSortedDownstream(downstream) =>
        // if there's a left neighbor, it needs the latest downstream
        for (leftNeighbor <- state.maybeLeftNeighbor) {
          leftNeighbor !! ReceiveSortedDownstream(downstream.prepend(self))
        }

        sorted(state, downstream.toList)

      case NewNeighbor(newNeighbor, retainSort, maybeExternalReplyTo) =>
        newNeighbor match {
          case Left(None) =>
            val updatedState = state.withUpdatedLeft(None)
            if (retainSort) {
              ???
            } else {
              awaitingSortedDownstream(updatedState, maybeExternalReplyTo)
            }
          case Left(leftNeighbor) =>
            val updatedState = state.withUpdatedLeft(leftNeighbor)
            if (retainSort) {
              sorted(updatedState, sortedDownstream)
            } else {
              waiting(updatedState)
            }
          case Right(maybeRightNeighbor) =>
            if (retainSort) ???
            val updatedState = state.withUpdatedRight(maybeRightNeighbor)
            maybeRightNeighbor match {
              case None => sorted(updatedState, Nil)
              case Some(rightNeighbor) =>
                rightNeighbor !! GetDownstreamListContents(context.messageAdapter(ReceiveSortedDownstream))
                awaitingSortedDownstream(state.withUpdatedRight(maybeRightNeighbor), maybeExternalReplyTo)
            }
        }
    }
  }

  //

  // returns (newLeft, remainingSortedDownstream) where remainingSortedDownstream.headOption is the newRight
  private def findNewDownstreamNeighbors(value: Int, sortedDownstream: NonEmptyList[CellWrapper]): (CellWrapper, List[CellWrapper]) = {
    @tailrec
    def helper(remaining: NonEmptyList[CellWrapper]): (CellWrapper, List[CellWrapper]) = {
      remaining match {
        case NonEmptyList(justOne, Nil) => (justOne, Nil)
        case NonEmptyList(maybeOurNewLeft, newDownstream @ maybeOurNewRight :: _) if maybeOurNewRight.value >= value =>
          (maybeOurNewLeft, newDownstream)
        case NonEmptyList(_, head :: tail) =>
          helper(NonEmptyList(head, tail))
        case _ => ???
      }
    }

    helper(sortedDownstream)
  }

  //

  private case class CellState(id: Int, index: Int, value: Int, maybeLeftNeighbor: Option[CellWrapper], maybeRightNeighbor: Option[CellWrapper]) {
    def withUpdatedRight(newRight:  Option[CellWrapper]): CellState = this.copy(maybeRightNeighbor = newRight)
    def withUpdatedLeft(newLeft:  Option[CellWrapper]): CellState = this.copy(maybeLeftNeighbor = newLeft)

    def probe: Probe.RecordLatest = Probe.RecordLatest(
      id = id,
      filename = SelfSortingArrays.filename(id, value),
      maybeLeft = maybeLeftNeighbor.map(_.noteName),
      maybeRight = maybeRightNeighbor.map(_.noteName)
    )
  }

  // convenience, makes simple comparisons less async

  // id is its starting index
  case class CellWrapper(id: Int, value: Int, noteName: String, spiritRef: SpiritRef[Cell.Message]) {
    def !!(message: Cell.Message)(implicit tc: TinkerContext[?]): Unit = {
      spiritRef !! message
    }
  }
}
