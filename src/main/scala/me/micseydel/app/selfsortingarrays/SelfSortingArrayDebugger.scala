package me.micseydel.app.selfsortingarrays

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Environment.InvariantViolation
import me.micseydel.app.selfsortingarrays.Probe.CellProbeState
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.{CompleteSwap, InsertionSortCellWrapper, NotifyOfSwap}
import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef

import scala.annotation.tailrec
import scala.util.{Failure, Success}

object SelfSortingArrayDebugger {
  sealed trait Message

  final case class UpdatedState(id: Int, probe: CellProbeState) extends Message
  final case class ClockTick(count: Int) extends Message
  final case class FoundABug(details: String) extends Message

  // this trait allows MessageSend and the synthetic events below to be added to a list
  sealed trait SequenceDiagramEvents

  final case class MessageSend(senderId: Option[Int], recipientId: Int, msg: String) extends Message with SequenceDiagramEvents

  // non-messages

  private case class StateChangeMoved(id: Int, movedRight: Boolean) extends SequenceDiagramEvents
  private case class StateChangeNewNeighbor(id: Int, newNeighbor: Either[Option[Int], Option[Int]]) extends SequenceDiagramEvents

  // behavior

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("SelfSortingArrayDebugger", TinkerColor.random(), "🐜") { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.setMarkdown("starting...\n") match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }
    behavior(Map.empty, Nil, Nil)
  }

  private def behavior(state: Map[Int, CellProbeState], sequenceDiagramEvents: List[SequenceDiagramEvents], priorOrder: List[(Int, CellProbeState)])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case UpdatedState(id, newProbe) =>
        val latestMessages: List[SequenceDiagramEvents] = state.get(id) match {
          case Some(existing@CellProbeState(maybeLeft, maybeRight, index, _)) if maybeLeft.nonEmpty || maybeRight.nonEmpty =>
            if (newProbe != existing) {
              if (newProbe.index == index + 1) {
                // we moved right
                StateChangeMoved(id, movedRight = true) :: sequenceDiagramEvents
              } else if (newProbe.index == index - 1) {
                // we moved left
                StateChangeMoved(id, movedRight = false) :: sequenceDiagramEvents
              } else if (newProbe.maybeLeft != maybeLeft) {
                // left updated
                StateChangeNewNeighbor(id, Left(newProbe.maybeLeft.map(_.id))) :: sequenceDiagramEvents
              } else if (newProbe.maybeRight != maybeRight) {
                // right updated
                StateChangeNewNeighbor(id, Right(newProbe.maybeRight.map(_.id))) :: sequenceDiagramEvents
              } else {
                throw InvariantViolation(s"new: ${newProbe.simpleString} old: ${existing.simpleString}")
              }
            } else sequenceDiagramEvents
          case _ =>
            sequenceDiagramEvents
        }
        behavior(state.updated(id, newProbe), latestMessages, priorOrder)

      case ClockTick(count) =>
        if (count == 0) {
          // initialized, but let's treat 0-1 as a big single frame
          behavior(state, sequenceDiagramEvents, state.toList.sortBy(_._1))
        } else {
          val orderedByCurrentIndex: List[(Int, CellProbeState)] = state.toList.sortBy(_._2.index)
          val currentOrder: List[Int] = orderedByCurrentIndex.map(_._1)

          val priorOrderToUse: List[(Int, CellProbeState)] = if (priorOrder.nonEmpty) {
            priorOrder
          } else {
            state.toList.sortBy(_._1)
          }

          val sequenceDiagram = mermaidSequenceDiagram(priorOrderToUse, sequenceDiagramEvents.reverse)

          ORACLE.drop(count).headOption match {
            case Some(ClockTickExpectation(currentOracle, expectedMessage)) =>
              if (currentOrder == currentOracle) {
                noteRef.appendLine2(s"# t=$count\n\n$sequenceDiagram").foreach(throw _)
              } else {
                noteRef.appendLine2(s"""# t=$count\n\n$sequenceDiagram\n\n- ⏰($count) ${currentOrder.mkString("\\[", ", ", "]")} (❌should have been ${currentOracle.mkString("\\[", ", ", "]")})""").foreach(throw _)
              }
            case None =>
              noteRef.appendLine2(s"# t=$count\n\n$sequenceDiagram\n\n- ⏰($count) ${currentOrder.mkString("\\[", ", ", "]")}").foreach(throw _)
          }
//          println(s"changing prior order from ${priorOrder.map(_._1)} to ${orderedByCurrentIndex.map(_._1)}")
          behavior(state, Nil, orderedByCurrentIndex)
        }

      case m@MessageSend(_, _, _) =>
        behavior(state, m :: sequenceDiagramEvents, priorOrder)

      case FoundABug(details) =>
        noteRef.appendLine2(s"- $details").foreach(throw _)
        Tinker.steadily
    }
  }

  private sealed trait Participant {
    def participantId: String
    def prettyName: String
  }

  private case object Environment extends Participant {
    override def participantId: String = "env"
    override def prettyName: String = participantId
  }

  private case class Cell(state: CellProbeState) extends Participant {
    override def participantId: String = s"C${state.id}"

    override def prettyName: String = {
      val maybeLeft = state.maybeLeft.map(_.id)
      val maybeRight = state.maybeRight.map(_.id)

      val elaboration = (maybeLeft, maybeRight) match {
        case (None, None) => "UNINITIALIZED?!"
        case (Some(left), None) => s"<- $left"
        case (None, Some(right)) => s"$right ->"
        case (Some(left), Some(right)) => s"<- $left | $right ->"
      }

      s"""${state.id} (val=${state.immutableCellProbeState.value})<br/>INDEX=${state.index}<br/>$elaboration"""
    }
  }

  private def mermaidSequenceDiagram(participatingCells: List[(Int, CellProbeState)], buffered: List[SequenceDiagramEvents]): String = {
//    println(s"using order ${participatingCells.map(_._1)}")

    val participantCells: List[Participant] = participatingCells.map { case (cellId, v) =>  Cell(v) }
    val msgFromGod = buffered.collect { case ms: MessageSend => ms } .exists(_.senderId.isEmpty)
    val participants: List[Participant] = if (msgFromGod) {
      Environment :: participantCells
    } else {
      participantCells
    }

    val messagesAndNotes: List[String] = buffered.flatMap {
      case MessageSend(Some(senderId), recipientId, msg) =>
        val mermaidSend = s"    C$senderId->>C$recipientId: $msg"
        if (msg.contains("Begin")) {
          List(
            s"activate C$senderId",
            s"$mermaidSend"
          )
        } else if (msg.contains("Complete")) {
          List(
            s"$mermaidSend",
            s"deactivate C$recipientId"
          )
        } else {
          List(mermaidSend)
        }
      case MessageSend(None, recipientId, msg) =>
        List(s"    env-->>C$recipientId: $msg")
      case StateChangeMoved(id, movedRight) =>
        if (movedRight) {
          val rightNeighbor = findRightNeighbor(id, participatingCells)
          List(
            s"Note right of C$id: ->",
            s"Note over C$id,C$rightNeighbor: SWAPPED"
          )
        } else {
          List(s"Note left of C$id: <-")
        }
      case StateChangeNewNeighbor(id, newNeighbor) =>
        List(newNeighbor match {
          case Left(newLeft) => s"Note left of C$id: NEW LEFT $newLeft"
          case Right(newRight) => s"Note right of C$id: NEW RIGHT $newRight"
        })
    }

    // FIXME: HACKY!!
    val (sorters, sortees) = messagesAndNotes.filter(_.contains("SWAPPED")).foldLeft((Nil: List[String], Nil: List[String])) {
      case ((sorters, sortees), line) =>
        // e.g. "Note over C1,C2: SWAPPED"
        val hacky = line.trim.drop("Note over C".getBytes.length)
        val leftSorter = "C" + hacky.take(1)
        val rightSortee = "C" + hacky.slice(3, 4)
        (leftSorter :: sorters, rightSortee :: sortees)
    }

    val mermaidParticipants = participants
      .map { participant =>
        if (sorters.contains(participant.participantId)) {
          // FIXME: random colors is FUGLY
          s"""box ${TinkerColor.random()}
             |    participant ${participant.participantId} as ${participant.prettyName}""".stripMargin
        } else if (sortees.contains(participant.participantId)) {
          s"""    participant ${participant.participantId} as ${participant.prettyName}
             |end""".stripMargin
        } else {
          s"    participant ${participant.participantId} as ${participant.prettyName}"
        }
      }
      .mkString("\n")

    s"""```mermaid
      |sequenceDiagram
      |$mermaidParticipants
      |${messagesAndNotes.mkString("\n")}
      |```""".stripMargin
  }

  @tailrec
  def findRightNeighbor(left: Int, participatingCells: List[(Int, CellProbeState)]): Int = {
    participatingCells match {
      case (id, _) :: (rightNeighborId, _) :: _ if id == left => rightNeighborId
      case _ :: tail => findRightNeighbor(left, tail)
      case _ => ???
    }
  }

//  private def terseTransition(id: Int, priorState: CellProbeState, newState: CellProbeState): String = {
//    def f(t: Option[InsertionSortCellWrapper]): String = t.map(_.id.toString).getOrElse("x")
//
//    s"$id (${f(priorState.maybeLeft)}, ${f(priorState.maybeRight)}) -> (${f(newState.maybeLeft)}, ${f(newState.maybeRight)})"
//  }

  case class ClockTickExpectation(theList: List[Int], expectedMessages: List[?])

//  case class MessageExpecter(messages: List[InsertionSortCell.Message]) {
//    def integrate(message: InsertionSortCell.Message): Unit = {
//      ???
//    }
//  }

  val ORACLE: List[ClockTickExpectation] = List(
    // FIXME: add expected messages!
    //  - create a MessageExpecter object, which can return updated state (like a Behavior!)
    ClockTickExpectation(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Nil), // 0
    ClockTickExpectation(List(0, 2, 1, 3, 4, 5, 6, 7, 8, 9), List(
//      NotifyOfSwap(),
//      CompleteSwap(),
//      NotifyOfSwap()
    )),
    ClockTickExpectation(List(2, 0, 3, 1, 4, 5, 6, 7, 8, 9), Nil /*FIXME*/),
    ClockTickExpectation(List(2, 3, 0, 4, 1, 5, 6, 7, 8, 9), Nil /*FIXME*/), // 3 ✅
    ClockTickExpectation(List(2, 3, 4, 0, 1, 6, 5, 7, 8, 9), Nil /*FIXME*/), // 4 ❌ List(2, 3, 4, 0, 1, **5, 6,** 7, 8, 9)
    ClockTickExpectation(List(2, 4, 3, 0, 1, 6, 7, 5, 8, 9), Nil /*FIXME*/), // 5 ❌ List(2, 4, 3, 0, 1,   5, 6,   7, 8, 9)
    ClockTickExpectation(List(2, 4, 3, 0, 1, 7, 6, 8, 5, 9), Nil /*FIXME*/), // 6
    ClockTickExpectation(List(2, 4, 3, 0, 7, 1, 8, 6, 9, 5), Nil /*FIXME*/), // 7
    ClockTickExpectation(List(2, 4, 3, 7, 0, 8, 1, 6, 9, 5), Nil /*FIXME*/), // 8
  )
}
