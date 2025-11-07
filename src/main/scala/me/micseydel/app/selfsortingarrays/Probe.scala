package me.micseydel.app.selfsortingarrays

import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.util.FileSystemUtil
import me.micseydel.vault.CanvasJsonFormats.canvasDataFormat
import me.micseydel.vault.{AllCanvasNodeData, CanvasData, CanvasEdgeData, CanvasFileData}
import spray.json.enrichAny

import java.nio.file.Path

object Probe {
  sealed trait Message

  final case class RecordLatest(id: Int, filename: String, maybeLeft: Option[String], maybeRight: Option[String]) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = behavior(Map.empty)

  private def behavior(state: Map[Int, (Option[String], String, Option[String])])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    Tinker.receiveMessage {
      case RecordLatest(id, filename, maybeLeft, maybeRight) =>
        val updatedState = state.updated(id, (maybeLeft, filename, maybeRight))

        val latestNodes = updatedState.toList.sortBy(_._1).zipWithIndex.map {
          case ((_, (mayyybeLeft, noteName, mayyybeRight)), i) =>
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
                maybeNeighborNoteName = maybeRight,
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
                maybeNeighborNoteName = maybeLeft,
                fromSide = fromSide,
                toSide = toSide
              )
            }

            withMaybeLeft
        }

        val withMutualEdgesDeduped =  edges.groupBy {
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
          Path.of("/Users/micseydel/obsidian_vaults/deliberate_knowledge_accretion/self_sorting_arrays/Self Sorting Lists.canvas"),
          canvas.toJson.prettyPrint
        )

        behavior(updatedState)
    }
  }

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
}
