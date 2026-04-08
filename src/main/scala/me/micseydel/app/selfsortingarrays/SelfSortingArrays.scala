package me.micseydel.app.selfsortingarrays

import akka.actor.typed.DispatcherSelector
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.app.AppConfiguration
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.{DefaultValueList, SelfSortingArrayCentralCast}
import me.micseydel.app.selfsortingarrays.cell.{BubbleSortCell, InsertionSortCell}
import me.micseydel.app.selfsortingarrays.cell.BubbleSortCell.{BubbleSortCellWrapper, Initialize}
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.InsertionSortCellWrapper
import me.micseydel.app.selfsortingarrays.support.{CellWrapper, Probe}
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.util.TimeUtil
import me.micseydel.vault.{Note, VaultPath}
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import net.jcazevedo.moultingyaml.*

import java.time.ZonedDateTime
import scala.annotation.{tailrec, unused}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object SelfSortingArrays {
  // https://github.com/Zhangtaining/cell_research/blob/1fd2bd5921c1f6b423a71f691d5189106a8a1020/sorting_cells.py#L4
  val DefaultValueList: NonEmptyList[Int] = NonEmptyList.of(28, 34, 6, 20, 7, 89, 34, 18, 29, 51)

  def main(args: Array[String]): Unit = {

    // FIXME: FYI this requires the path environment variable:- vaultRoot
    AppConfiguration.getConfig() match {
      case Validated.Invalid(errors) =>
        println(s"FAILED, errors-\n$errors")
      case Validated.Valid(config: AppConfig) =>
        println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Starting system: config with vault root ${config.vaultRoot}, creating json/ subdirectory if needed")

        // keeps the system alive waiting for messages on a separate thread
        @unused
        val container =
        TinkerContainer(config, NotificationCenterAbilities.None)(
          centralCastFactory(config.vaultRoot)(_, _), // effectively globals
          Container()(_: EnhancedTinker[SelfSortingArrayCentralCast])
        )
    }
  }

  def centralCastFactory(vaultPath: VaultPath)(implicit Tinker: Tinker, context: TinkerContext[?]): SelfSortingArrayCentralCast = {
    val probe = context.cast(Probe(vaultPath), "Probe")
    SelfSortingArrayCentralCast(probe)
  }

  case class SelfSortingArrayCentralCast(probe: SpiritRef[Probe.Message])

  def filename(id: Int, value: Int): String = {
    s"Cell $id ($value)"
  }
}

object Container {
  sealed trait Message

  private final case class NotePing(ping: Ping) extends Message

  private case object ClockTick extends Message

  def apply()(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = AttentiveNoteMakingTinkerer[Message, NotePing]("Self Sorting Arrays Probe", TinkerColor.random(), "🐄", NotePing) { (context, noteRef) =>
    implicit val probe: SpiritRef[Probe.Message] = Tinker.userExtension.probe
    implicit val tc: TinkerContext[?] = context

    probe !! Probe.RegisterEnvironment(context.self)

    noteRef.setDefaultDocument() match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    implicit val nr: NoteRef = noteRef
    waiting()
  }

  private def waiting()(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case NotePing(_) =>
        noteRef.readValidatedDocument() match {
          case Validated.Valid(Document(Config(head :: tail, startDelaySeconds, clockTickSeconds), bubbleSortBoxChecked, insertionSortBoxChecked)) if !(bubbleSortBoxChecked && insertionSortBoxChecked) =>
            val values: NonEmptyList[Int] = NonEmptyList(head, tail)
            if (bubbleSortBoxChecked) {
              implicit val cells: NonEmptyList[CellWrapper[BubbleSortCell.Message]] = createBubbleSortCells(values)
              cells.head !!! BubbleSortCell.DoSort

              val timeKeeper = context.castTimeKeeper()
              timeKeeper !! TimeKeeper.RemindMeEvery(clockTickSeconds.seconds, startDelaySeconds.seconds, context.self, ClockTick, None)

              bubbleSort(0)
            } else if (insertionSortBoxChecked) {
              implicit val cells: NonEmptyList[CellWrapper[InsertionSortCell.Message]] = createInsertionSortCells(values)
              cells.head !!! InsertionSortCell.DoSort
              insertionSort(0)
            } else {
              Tinker.steadily
            }

          case other =>
            context.actorContext.log.warn(s"Not sure what to do with: $other")
            Tinker.steadily
        }

      case ClockTick => ???
    }
  }

  private def bubbleSort(clockTick: Int)
                        (implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef, probe: SpiritRef[Probe.Message], cells: NonEmptyList[CellWrapper[BubbleSortCell.Message]]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case NotePing(_) => Tinker.steadily
      case ClockTick =>
        probe !! Probe.ClockTick(clockTick)
        cells.toList.foreach(_ !!! BubbleSortCell.ClockTick(clockTick))
        bubbleSort(clockTick + 1)
    }
  }

  private def createBubbleSortCells(valueList: NonEmptyList[Int])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], context: TinkerContext[?], probe: SpiritRef[Probe.Message]): NonEmptyList[CellWrapper[BubbleSortCell.Message]] = {
    val zipped = valueList.zipWithIndex.map { case (int, startIndex) =>
      (startIndex, SelfSortingArrays.filename(startIndex, int), int)
    }

    val cells: NonEmptyList[CellWrapper[BubbleSortCell.Message]] = zipped.map {
      case (index, filename, value) =>
        val actorName = filename.replace(" ", "_").replace("(", "").replace(")", "")
        CellWrapper(
          index,
          value,
          filename,
          context.cast(
            BubbleSortCell(index, index, filename, value), actorName,
            SingleThreadDispatcher
          )
        )
    }

    @tailrec
    def finishInitializingCells(remaining: List[Option[BubbleSortCellWrapper]]): Unit = {
      remaining match {
        case left :: Some(cell) :: right :: _ =>
          cell !!! BubbleSortCell.Initialize(left, right)
          finishInitializingCells(remaining.tail)

        case List(_, _) => // done!

        case _ => ???
      }
    }

    finishInitializingCells(None :: cells.toList.map(Some(_)) ::: List(None))

    cells
  }

  private def insertionSort(clockTick: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef, probe: SpiritRef[Probe.Message], cells: NonEmptyList[CellWrapper[InsertionSortCell.Message]]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case NotePing(_) => Tinker.steadily
      case ClockTick =>
        probe !! Probe.ClockTick(clockTick)
        insertionSort(clockTick + 1)
    }
  }

  private def createInsertionSortCells(valueList: NonEmptyList[Int])
                                      (implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], context: TinkerContext[?], probe: SpiritRef[Probe.Message]): NonEmptyList[InsertionSortCellWrapper] = {
    val zipped = valueList.zipWithIndex.map { case (int, startIndex) =>
      (startIndex, SelfSortingArrays.filename(startIndex, int), int)
    }

    val cells: NonEmptyList[InsertionSortCellWrapper] = zipped.map {
      case (index, filename, value) =>
        val actorName = filename.replace(" ", "_").replace("(", "").replace(")", "")
        CellWrapper(
          index,
          value,
          filename,
          context.cast(
            InsertionSortCell(index, index, filename, value), actorName,
            SingleThreadDispatcher
          )
        )
    }

    @tailrec
    def finishInitializingCells(remaining: List[Option[InsertionSortCellWrapper]]): Unit = {
      remaining match {
        case left :: Some(cell) :: right :: _ =>
          cell !!! InsertionSortCell.Initialize(left, right)
          finishInitializingCells(remaining.tail)

        case List(_, _) => // done!

        case _ => ???
      }
    }

    finishInitializingCells(None :: cells.toList.map(Some(_)) ::: List(None))

    cells
  }


  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def setMarkdownOrThrow(markdown: String): Unit = noteRef.setMarkdown(markdown) match {
      case Failure(exception) => throw exception
      case Success(NoOp) =>
    }

    def setDefaultDocument(): Try[NoOp.type] = {
      val frontmatter =
        s"""listToSort: ${DefaultValueList.toList.mkString("[", ", ", "]")}
           |startDelaySeconds: 1
           |clockTickSeconds: 1
           |""".stripMargin
      noteRef
        .setTo(Note(DefaultMarkdown, Some(frontmatter)))
        .map(_ => NoOp)
    }

    def readValidatedDocument(): ValidatedNel[String, Document] = {
      import YamlProtocol.configYamlFormat

      // FIXME: too simple, needs error handling
      def checkedBoxes(markdown: String) = markdown.split("\n").take(2).toList match {
        case List(bubbleSortLine, insertionSortLine) =>
          (
            bubbleSortLine.length > 3 && bubbleSortLine(3) != ' ',
            insertionSortLine.length > 3 && insertionSortLine(3) != ' '
          )

        case other => (false, false)
      }

      noteRef.readNote().flatMap {
        case Note(markdown, None) =>
          val (bubbleSortChecked, insertionSortChecked) = checkedBoxes(markdown)
          Success(Document(Config(DefaultValueList.toList), bubbleSortChecked, insertionSortChecked))
        case Note(markdown, Some(frontmatter)) =>
          Try(frontmatter.parseYaml.convertTo[Config]).map { config =>
            val (bubbleSortChecked, insertionSortChecked) = checkedBoxes(markdown)
            Document(config, bubbleSortChecked, insertionSortChecked)
          }
      } match {
        case Failure(exception) => Common.getStackTraceString(exception).invalidNel
        case Success(document) => document.validNel
      }
    }
  }

  private val DefaultMarkdown =
    s"""- [ ] Bubble sort
       |- [ ] Insertion sort
       |
       |# Instructions
       |
       |- Obsidian graph view
       |    - **filter** `tag:#cell`
       |    - group tags and example rgb for bubble sort
       |        - tag:#initializing 234/200/220
       |        - tag:#initialized 92/139/214
       |        - tag:#active 51/210/20
       |        - tag:#InvariantViolation 233/30/7
       |        - tag:#inactive 255/174/0
       |    - display: arrows ON
       |    - forces: higher **repel force** and smaller **center force** make more spread out visualizations
       |- Potentially useful notes (once sorting starts)
       |    - each cell note e.g. [[Cell 0 (28)]]
       |    - [[SelfSortingArrayDebugger]] (rough sequence diagram)
       |    - [[Self Sorting Lists.canvas]] (better than the graph for manual exploration)
       |    - [[Notable Events]]
       |""".stripMargin

  //

  case class Config(
                     listToSort: List[Int],
                     startDelaySeconds: Int = 1,
                     clockTickSeconds: Int = 1,
                   )

  case class Document(
                       config: Config,
                       bubbleSortBoxChecked: Boolean,
                       insertionSortBoxChecked: Boolean,
                     )

  private object YamlProtocol extends DefaultYamlProtocol {
    implicit val configYamlFormat: YamlFormat[Config] = yamlFormat3(Config)
  }

  private def SingleThreadDispatcher: DispatcherSelector = DispatcherSelector.fromConfig("self-sorting-dispatcher")
}
