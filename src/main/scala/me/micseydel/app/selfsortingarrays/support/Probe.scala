package me.micseydel.app.selfsortingarrays.support

import me.micseydel.NoOp
import me.micseydel.app.selfsortingarrays.Container
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.util.FileSystemUtil
import me.micseydel.vault.CanvasJsonFormats.canvasDataFormat
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.vault.{CanvasData, CanvasEdgeData, CanvasFileData, VaultPath}
import spray.json.enrichAny

import java.nio.file.Path
import scala.util.{Failure, Success}

object Probe {
  sealed trait Message

  final case class RegisterCell(id: Int, value: Int, filename: String) extends Message

  final case class UpdatedState(id: Int, cellState: CellState) extends Message

  final case class MessageSend(senderId: Option[Int], recipientId: Int, msg: String) extends Message

  final case class ClockTick(count: Int) extends Message

  final case class FoundABug(details: String) extends Message

  final case class RegisterEnvironment(environment: SpiritRef[Container.Message]) extends Message

  def apply(vaultPath: VaultPath)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Notable Events", TinkerColor.random(), "🎭") { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.setMarkdown("waiting to start...\n\n") match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }
    implicit val debugger: SpiritRef[SelfSortingArrayDebugger.Message] = context.cast(SelfSortingArrayDebugger(), "SelfSortingArrayDebugger")
    behavior(Map.empty, None, vaultPath)
  }

  case class ImmutableCellProbeState(id: Int, value: Int, filename: String)

  case class CellProbeState(maybeLeft: Option[CellWrapper[?]], maybeRight: Option[CellWrapper[?]], index: Int, immutableCellProbeState: ImmutableCellProbeState) {

    def id: Int = immutableCellProbeState.id
    def value: Int = immutableCellProbeState.value

    def simpleString = s"""[$index] ${maybeLeft.map(_.id).getOrElse("x")} <-> ${maybeRight.map(_.id).getOrElse("x")}"""
  }

  private def behavior(state: Map[Int, CellProbeState], environment: Option[SpiritRef[Container.Message]], vaultPath: VaultPath)(implicit Tinker: Tinker, noteRef: NoteRef, debugger: SpiritRef[SelfSortingArrayDebugger.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case RegisterCell(id, value, filename) =>
        val probeState = CellProbeState(maybeLeft = None, maybeRight = None, index = id, ImmutableCellProbeState(id, value, filename))
        val updatedState: Map[Int, CellProbeState] =
          state.updated(id, probeState)
        debugger !! SelfSortingArrayDebugger.UpdatedState(id, probeState)
        behavior(updatedState, environment, vaultPath)

      case UpdatedState(id, CellState(newIndex, newLeftNeighbor, newRightNeighbor)) =>
        state.get(id) match {
          case Some(CellProbeState(None, None, oldIndex, _)) if newLeftNeighbor.isEmpty && newRightNeighbor.isEmpty && oldIndex == newIndex =>
            noteRef.appendLine2(s"""- $id uninitialized""") match {
              case Some(throwable) => throw throwable
              case None =>
            }
            Tinker.steadily
          case Some(oldCellState@CellProbeState(None, None, oldIndex, _)) if oldIndex == newIndex =>
            val updatedState = oldCellState.copy(index = newIndex, maybeLeft = newLeftNeighbor, maybeRight = newRightNeighbor)
            debugger !! SelfSortingArrayDebugger.UpdatedState(id, updatedState)
            noteRef.appendLine2(s"""- $id initialized (${newLeftNeighbor.map(_.id).getOrElse("x")}, ${newRightNeighbor.map(_.id).getOrElse("x")})""") match {
              case Some(throwable) => throw throwable
              case None =>
            }
            behavior(state.updated(id, updatedState), environment, vaultPath)
          case Some(oldCellState@CellProbeState(oldLeft, oldRight, oldIndex, _)) =>
            val updatedState = oldCellState.copy(index = newIndex, maybeLeft = newLeftNeighbor, maybeRight = newRightNeighbor)
            debugger !! SelfSortingArrayDebugger.UpdatedState(id, updatedState)
            noteRef.appendLine2(s"""- $id (${getOptionalInsertionSortCellWrapperIdOrX(oldLeft)}, ${getOptionalInsertionSortCellWrapperIdOrX(oldRight)}) -> (${newLeftNeighbor.map(_.id).getOrElse("x")}, ${newRightNeighbor.map(_.id).getOrElse("x")}); index $oldIndex -> $newIndex""") match {
              case Some(throwable) => throw throwable
              case None =>
            }
            behavior(state.updated(id, updatedState), environment, vaultPath)
          case None => ???
        }
      case MessageSend(senderId, recipientId, msg) =>
        debugger !! SelfSortingArrayDebugger.MessageSend(senderId, recipientId, msg)
        val sender = senderId.map(_.toString).getOrElse("env")
        noteRef.appendLine2(s"""- $sender->$recipientId: ${msg.replace("$$", "\\$$")}""") match {
          case Some(throwable) => throw throwable
          case None =>
        }
        Tinker.steadily

      case ClockTick(count) =>
        debugger !! SelfSortingArrayDebugger.ClockTick(count)
        noteRef.appendLine2(s"\n- ClockTick($count) ${currentOrder(state)}") match {
          case Some(throwable) => throw throwable
          case None =>
        }

        val latestNodes = state.toList.sortBy(_._2.index).zipWithIndex.map {
          case ((_, CellProbeState(mayyybeLeft, mayyybeRight, index, ImmutableCellProbeState(_, _, noteName))), i) =>
            wrapCanvasFileData(400 * i, noteName) -> (mayyybeLeft, mayyybeRight)
        }

        val edges = latestNodes.foldRight(Nil: List[CanvasEdgeData]) {
          case ((canvasNode, (maybeLeft, maybeRight)), edgesSoFar) =>

            val withMaybeRight = {
              val (fromSide, toSide) = if (maybeRight.nonEmpty) {
                ("right", "left")
              } else {
                ("bottom", "bottom")
              }
              appendCanvasEdgeData(
                appendTo = edgesSoFar,
                myNoteName = canvasNode.id,
                maybeNeighborNoteName = maybeRight.map(n => s"Cell ${n.id} (${n.value})"),
                fromSide = fromSide,
                toSide = toSide
              )
            }

            val withMaybeLeft = {
              val (fromSide, toSide) = if (maybeRight.nonEmpty) {
                ("left", "right")
              } else {
                ("bottom", "bottom")
              }
              appendCanvasEdgeData(
                appendTo = withMaybeRight,
                myNoteName = canvasNode.id,
                maybeNeighborNoteName = maybeLeft.map(n => s"Cell ${n.id} (${n.value})"),
                fromSide = fromSide,
                toSide = toSide
              )
            }

            withMaybeLeft
        }

        val withMutualEdgesDeduped = edges.groupBy {
          case CanvasEdgeData(id, fromNode, fromSide, fromEnd, toNode, toSide, toEnd, color, label) =>
            List(fromNode, toNode).sorted.mkString("")
        }.map {
          case (_, l) =>
            l match {
              case Nil =>
                ???
              case List(singleDirection) =>
                singleDirection
              case List(oneEdge, secondEdge) =>
                CanvasEdgeData(
                  id = oneEdge.id + secondEdge.id,
                  fromNode = oneEdge.fromNode,
                  // FIXME: how to detect left->right vs right->left?
                  fromSide = "right",
                  fromEnd = Some("arrow"),
                  toNode = oneEdge.toNode,
                  toSide = "left",
                  toEnd = None,
                  color = None,
                  label = None
                )

              case other => ???
            }
        }.toList

        val canvas = CanvasData(latestNodes.map(_._1), withMutualEdgesDeduped)
        FileSystemUtil.writeToPath(
          // FIXME: call probe?
          vaultPath.resolve("Self Sorting Lists.canvas"),
          canvas.toJson.prettyPrint
        )
        Tinker.steadily

      case FoundABug(details) =>
        debugger !! SelfSortingArrayDebugger.FoundABug(details)
        context.actorContext.log.warn(s"bug, stopping the clock $environment")
        Tinker.steadily

      case RegisterEnvironment(newEnvironment) =>
        context.actorContext.log.debug(s"new env $newEnvironment")
        behavior(state, Some(newEnvironment), vaultPath)
    }
  }

  //

  private def wrapCanvasFileData(
                                  //                    id: String,
                                  x: Int,
                                  //                    y: Int,
                                  //                    width: Int,
                                  //                    height: Int,
                                  //                    color: Option[String],
                                  //                   `type`: String,
                                  noteName: String,
                                  //                    subpath: Option[String]
                                ): CanvasFileData = {
    CanvasFileData(
      id = noteName,
      x = x: Int,
      y = 0,
      width = 300,
      height = 400,
      color = None,
      `type` = "file",
      file = s"$noteName.md",
      subpath = None
    )
  }

  private def appendCanvasEdgeData(
                                    appendTo: List[CanvasEdgeData],
                                    myNoteName: String,
                                    maybeNeighborNoteName: Option[String],
                                    //                                  id: String,
                                    //                                  fromNode: String,
                                    fromSide: String,
                                    ////                                  fromEnd: Option[String],
                                    //                                  toNode: String,
                                    toSide: String,
                                    //                                  toEnd: Option[String],
                                    //                                  color: Option[String],
                                    //                                  label: Option[String]
                                  ): List[CanvasEdgeData] = {
    maybeNeighborNoteName match {
      case None => appendTo
      case Some(neighborNoteName) =>
        if (myNoteName == neighborNoteName) {
          throw new RuntimeException(s"myNoteName == neighborNoteName ($myNoteName == $neighborNoteName)")
        }

        val id = s"$myNoteName->$neighborNoteName"

        CanvasEdgeData(
          id = id,
          fromNode = myNoteName,
          fromSide = fromSide,
          fromEnd = None,
          toNode = neighborNoteName,
          toSide = toSide,
          toEnd = None,
          color = None,
          label = None
        ) :: appendTo
    }
  }

  def getOptionalInsertionSortCellWrapperIdOrX(t: Option[CellWrapper[?]]): String = t.map(_.id.toString).getOrElse("x")

  def currentOrder(state: Map[Int, CellProbeState]): List[Int] = state.toList.sortBy(_._2.index).map(_._1)
}
