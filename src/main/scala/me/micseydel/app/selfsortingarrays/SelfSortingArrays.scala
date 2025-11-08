package me.micseydel.app.selfsortingarrays

import cats.data.{NonEmptyList, Validated}
import me.micseydel.NoOp
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.app.AppConfiguration
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.InsertionSortCellWrapper
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.SelfSortingArrayCentralCast
import me.micseydel.app.selfsortingarrays.cell.{CellWrapper, InsertionSortCell}
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import scala.annotation.unused
import scala.util.{Failure, Success}

object SelfSortingArrays {
  // https://github.com/Zhangtaining/cell_research/blob/1fd2bd5921c1f6b423a71f691d5189106a8a1020/sorting_cells.py#L4
  //  private val VALUE_LIST = NonEmptyList.of(28, 34, 6, 20, 7, 89, 34, 18, 29, 51)

  private val VALUE_LIST = NonEmptyList.of(
    28, 34, 6, 20, 7,
    89, // 18, 29, 34, 51
    // works up to 34
    34, 18, 29, 51)

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
          centralCastFactory()(_, _), // effectively globals
          Environment(VALUE_LIST)(_: EnhancedTinker[SelfSortingArrayCentralCast])
        )
    }
  }

  def centralCastFactory()(implicit Tinker: Tinker, context: TinkerContext[?]): SelfSortingArrayCentralCast = {
    val probe = context.cast(Probe(), "Probe")
    SelfSortingArrayCentralCast(probe)
  }

  case class SelfSortingArrayCentralCast(probe: SpiritRef[Probe.Message])

  def filename(id: Int, value: Int): String = {
    s"Cell $id ($value)"
  }
}


object Environment {
  sealed trait Message

  private final case class ReceiveListContents(contents: NonEmptyList[InsertionSortCellWrapper]) extends Message

  private final case class NotePing(ping: Ping) extends Message

  def apply(valueList: NonEmptyList[Int])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = AttentiveNoteMakingTinkerer[Message, NotePing]("Self Sorting Arrays Probe", TinkerColor.random(), "🐄", NotePing) { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context


    noteRef.setMarkdown(s"Initializing with $valueList") match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }

    val zipped = valueList.zipWithIndex.map { case (int, startIndex) =>
      (startIndex, SelfSortingArrays.filename(startIndex, int), int)
    }

    // this acts like the head of a linked list
    val cellWrapper = zipped.head match {
      case (cell_starting_index, filename, value) =>
        CellWrapper(cell_starting_index, value, filename, context.cast(InsertionSortCell(cell_starting_index, 0, filename, value, zipped.tail), filename.replace(" ", "_").replace("(", "").replace(")", "")))
    }

    println("Requesting list contents")
    cellWrapper !! InsertionSortCell.GetDownstreamListContents(context.messageAdapter(ReceiveListContents))

    implicit val nr: NoteRef = noteRef
    waitingForOriginalList(cellWrapper)
  }

  private def waitingForOriginalList(listHead: InsertionSortCellWrapper)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case ReceiveListContents(listContents) =>
        val values = listContents.map(_.value)
        noteRef.setMarkdown(s"- [ ] Check to sort $values") match {
          case Failure(exception) => throw exception
          case Success(_) =>
        }
        val msg = s"Starting list: $values"
        println(msg)
        val newHead = listContents.head
        if (newHead.spiritRef.path != listHead.spiritRef.path) {
          println(s"WARNING no sorting so listHead ${listHead.spiritRef.path} should not have changed to ${newHead.spiritRef.path}")
        }
        println("Waiting for sort request")
        waiting(listHead)

      case NotePing(_) => Tinker.steadily // ignore
    }
  }

  private def waiting(listHead: InsertionSortCellWrapper)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case ReceiveListContents(listContents) =>
        val values = listContents.map(_.value)
        noteRef.setMarkdown(s"Sorted list: $values") match {
          case Failure(exception) => throw exception
          case Success(_) =>
        }
        val msg = s"FINISHED!!! Head ${listHead.value}->${listContents.head.value}, sorted contents: $values"
        println(msg)
        Tinker.steadily

      case NotePing(_) =>
        if (noteRef.checkBoxIsChecked()) {
          println("Starting sort request!")
          listHead !! InsertionSortCell.RequestSortedContents(context.messageAdapter(ReceiveListContents))
        }
        Tinker.steadily
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown()
        .map(markdown => {
          //          println(markdown)
          markdown.startsWith("- [x] ")
        }) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }
}


