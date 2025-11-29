package me.micseydel.app.selfsortingarrays.pureakka

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import cats.data.NonEmptyList

import scala.annotation.{tailrec, unused}

object AkkaSelfSortingArrays {
  // https://github.com/Zhangtaining/cell_research/blob/1fd2bd5921c1f6b423a71f691d5189106a8a1020/sorting_cells.py#L4
  private val VALUE_LIST = NonEmptyList.of(28, 34, 6, 20, 7, 89, 34, 18, 29, 51)

  def main(args: Array[String]): Unit = {
    // keeps the system alive waiting for messages on a separate thread
    @unused
    val actorSystem = ActorSystem(AkkaEnvironment(VALUE_LIST), "SelfSortingArrays")
  }
}

object AkkaEnvironment {
  sealed trait Message

  private final case class ReceiveListContents(contents: NonEmptyList[AkkaCell.AkkaCellWrapper]) extends Message


  def apply(valueList: NonEmptyList[Int]): Behavior[Message] = Behaviors.setup { context =>
    val zipped = valueList.zipWithIndex.map { case (int, startIndex) =>
      (startIndex, s"Cell $startIndex ($int)", int)
    }

    // this acts like the head of a linked list
    val cellWrapper = zipped.head match {
      case (cell_starting_index, filename, value) =>
        AkkaCell.AkkaCellWrapper(cell_starting_index, value, filename, context.spawn(AkkaCell(cell_starting_index, 0, filename, value, zipped.tail), filename.replace(" ", "_").replace("(", "").replace(")", "")))
    }

    println("Requesting list contents")
    cellWrapper ! AkkaCell.GetDownstreamListContents(context.messageAdapter(ReceiveListContents))

    waitingForOriginalList(cellWrapper)
  }

  private def waitingForOriginalList(listHead: AkkaCell.AkkaCellWrapper): Behavior[Message] = Behaviors.receive {
    case (context, ReceiveListContents(listContents)) =>
      val values = listContents.map(_.value)
      val msg = s"Starting list: $values"
      println(msg)
      val newHead = listContents.head
      if (newHead.actorRef.path != listHead.actorRef.path) {
        println(s"WARNING no sorting so listHead ${listHead.actorRef.path} should not have changed to ${newHead.actorRef.path}")
      }
      listHead ! AkkaCell.RequestSortedContents(context.messageAdapter(ReceiveListContents))
      println("Waiting for sort request")
      waiting(listHead)
  }

  private def waiting(listHead: AkkaCell.AkkaCellWrapper): Behavior[Message] = Behaviors.receiveMessage {
    case ReceiveListContents(listContents) =>
      val values = listContents.map(_.value)
      val msg = s"FINISHED!! Head ${listHead.value}->${listContents.head.value}, sorted contents: $values"
      println(msg)
      Behaviors.same
  }
}


object AkkaCell {

  // inbox 📥

  sealed trait Message

  sealed trait MessageToRespondTo extends Message {
    def replyTo: ActorRef[NonEmptyList[AkkaCellWrapper]]
  }

  final case class GetDownstreamListContents(replyTo: ActorRef[NonEmptyList[AkkaCellWrapper]]) extends MessageToRespondTo

  final case class RequestSortedContents(replyTo: ActorRef[NonEmptyList[AkkaCellWrapper]]) extends MessageToRespondTo

  private case class NewNeighbor(leftOrRight: Either[Option[AkkaCellWrapper], Option[AkkaCellWrapper]],
                                 retainSort: Boolean = false,
                                 maybeExternalReplyTo: Option[ActorRef[NonEmptyList[AkkaCellWrapper]]] = None
                                ) extends Message

  private case class ReceiveSortedDownstream(sortedDownstream: NonEmptyList[AkkaCellWrapper]) extends Message

  // behavior 😇

  def apply(id: Int, index: Int, noteName: String, value: Int, remaining: List[(Int, String, Int)]): Behavior[Message] = {
    setup(id, index, noteName, value, remaining, None)
  }

  private def setup(id: Int, index: Int, noteName: String, value: Int, remaining: List[(Int, String, Int)], leftNeighbor: Option[AkkaCellWrapper]): Behavior[Message] = Behaviors.setup { context =>
    implicit val self: AkkaCellWrapper = AkkaCellWrapper(id, value, noteName, context.self)
    remaining match {
      case Nil =>
        waiting(CellState(index, leftNeighbor, None))
      case (nextId, nextNoteName, nextValue) :: tail =>
        val rightNeighbor = context.spawn(AkkaCell.setup(nextId, index+1, nextNoteName, nextValue, tail, leftNeighbor = Some(self)), nextNoteName.replace(" ", "_").replace("(", "").replace(")", ""))
        waiting(CellState(index, leftNeighbor, Some(AkkaCellWrapper(nextId, nextValue, nextNoteName, rightNeighbor))))
    }
  }

  private def waiting(state: CellState)(implicit self: AkkaCellWrapper): Behavior[Message] = {
    if (state.maybeLeftNeighbor.contains(self)) {
      throw new RuntimeException(s"${state.maybeLeftNeighbor} contains self!")
    }
    if (state.maybeRightNeighbor.contains(self)) {
      throw new RuntimeException(s"${state.maybeRightNeighbor} contains self!")
    }

    Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case GetDownstreamListContents(replyTo) =>
          state.maybeRightNeighbor match {
            case None =>
              replyTo ! NonEmptyList.of(self)
              Behaviors.same
            case Some(rightNeighbor) =>
              rightNeighbor ! GetDownstreamListContents(context.messageAdapter(ReceiveSortedDownstream))
              awaitingAggregation(state, replyTo)
          }

        case RequestSortedContents(replyTo) =>
          state.maybeRightNeighbor match {
            case None =>
              replyTo ! NonEmptyList.of(self)
              sorted(state, Nil)
            case Some(rightNeighbor) =>
              rightNeighbor ! RequestSortedContents(context.messageAdapter(ReceiveSortedDownstream))
              awaitingSortedDownstream(state, Some(replyTo))
          }

        case ReceiveSortedDownstream(sortedDownstream) =>
          sorted(state.withUpdatedRight(Some(sortedDownstream.head)), sortedDownstream.toList)

        case NewNeighbor(_, _, _) => ???
      }
    }
  }

  private def awaitingAggregation(state: CellState, replyTo: ActorRef[NonEmptyList[AkkaCellWrapper]])(implicit self: AkkaCellWrapper): Behavior[Message] = Behaviors.receiveMessage {
    case ReceiveSortedDownstream(downstream) =>
      replyTo ! downstream.prepend(self)
      waiting(state)

    case GetDownstreamListContents(_) => ???
    case RequestSortedContents(_) => ???
    case NewNeighbor(_, _, _) => ???
  }

  private def awaitingSortedDownstream(state: CellState, maybeCallerReplyTo: Option[ActorRef[NonEmptyList[AkkaCellWrapper]]])(implicit self: AkkaCellWrapper): Behavior[Message] = Behaviors.receiveMessage {
    case ReceiveSortedDownstream(downstream) =>
      val justNums = downstream.map(_.value).toList
      if (justNums != justNums.sorted) {
        println(s"[awaitingSortedDownstream ${self.value}] downstream not sorted: $justNums")
        ???
      }

      if (self.value > downstream.head.value) {
        findNewDownstreamNeighbors(self.value, downstream) match {
          case (newLeft, updatedDownstream) =>
            // notify our old neighbors of their new neighbors
            // FIXME: unfortunately, there's a race-condition here: if Right doesn't get this message before propagation left reaches it (unlikely), it will send its result to SELF instead of Left
            state.maybeRightNeighbor.foreach(_ ! NewNeighbor(Left(state.maybeLeftNeighbor), maybeExternalReplyTo = maybeCallerReplyTo))
            state.maybeLeftNeighbor.foreach(_ ! NewNeighbor(Right(Some(state.maybeRightNeighbor.get)))) // FIXME: sloppy .get, but receiving this message means we should have a right neighbor so this creates a thrown exception if we need it
            // notify our new neighbors - right is special, it will remain sorted
            newLeft ! NewNeighbor(Right(Some(self)))
            updatedDownstream.headOption.foreach(_ ! NewNeighbor(Left(Some(self)), retainSort = true))

            // now propagate the update toward the head
            newLeft ! ReceiveSortedDownstream(NonEmptyList(self, updatedDownstream))

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
            replyTo ! downstream.prepend(self)
          case None =>
            state.maybeLeftNeighbor match {
              case None => ???
              case Some(replyTo) =>
                replyTo ! ReceiveSortedDownstream(downstream.prepend(self))
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

  private def sorted(state: CellState, sortedDownstream: List[AkkaCellWrapper])(implicit self: AkkaCellWrapper): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case message: MessageToRespondTo =>
        message.replyTo ! NonEmptyList(self, sortedDownstream)
        Behaviors.same

      case ReceiveSortedDownstream(downstream) =>
        // if there's a left neighbor, it needs the latest downstream
        for (leftNeighbor <- state.maybeLeftNeighbor) {
          leftNeighbor ! ReceiveSortedDownstream(downstream.prepend(self))
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
                rightNeighbor ! GetDownstreamListContents(context.messageAdapter(ReceiveSortedDownstream))
                awaitingSortedDownstream(state.withUpdatedRight(maybeRightNeighbor), maybeExternalReplyTo)
            }
        }
    }
  }

  //

  // returns (newLeft, remainingSortedDownstream) where remainingSortedDownstream.headOption is the newRight
  private def findNewDownstreamNeighbors(value: Int, sortedDownstream: NonEmptyList[AkkaCellWrapper]): (AkkaCellWrapper, List[AkkaCellWrapper]) = {
    @tailrec
    def helper(remaining: NonEmptyList[AkkaCellWrapper]): (AkkaCellWrapper, List[AkkaCellWrapper]) = {
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

  private case class CellState(index: Int, maybeLeftNeighbor: Option[AkkaCellWrapper], maybeRightNeighbor: Option[AkkaCellWrapper]) {
    def withUpdatedRight(newRight:  Option[AkkaCellWrapper]): CellState = this.copy(maybeRightNeighbor = newRight)
    def withUpdatedLeft(newLeft:  Option[AkkaCellWrapper]): CellState = this.copy(maybeLeftNeighbor = newLeft)
  }

  // convenience, makes simple comparisons less async

  // id is its starting index
  case class AkkaCellWrapper(id: Int, value: Int, noteName: String, actorRef: ActorRef[AkkaCell.Message]) {
    def !(message: AkkaCell.Message): Unit = {
      actorRef ! message
    }
  }
}

