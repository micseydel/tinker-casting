package me.micseydel.app.selfsortingarrays

import akka.actor.typed.DispatcherSelector
import cats.data.{NonEmptyList, Validated}
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.app.AppConfiguration
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.app.selfsortingarrays.SelfSortingArrays.{SelfSortingArrayCentralCast, VALUE_LIST}
import me.micseydel.app.selfsortingarrays.cell.InsertionSortCell.{Initialize, InsertionSortCellWrapper}
import me.micseydel.app.selfsortingarrays.cell.{CellWrapper, InsertionSortCell}
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import scala.annotation.{tailrec, unused}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object SelfSortingArrays {
  // https://github.com/Zhangtaining/cell_research/blob/1fd2bd5921c1f6b423a71f691d5189106a8a1020/sorting_cells.py#L4
  val VALUE_LIST: NonEmptyList[Int] = NonEmptyList.of(
    28, 34, 6, 20, 7,
    89, 34, 18, 29, 51
  )

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

  private final case class NotePing(ping: Ping) extends Message
  private case object ClockTick extends Message

  def apply(valueList: NonEmptyList[Int])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast]): Ability[Message] = AttentiveNoteMakingTinkerer[Message, NotePing]("Self Sorting Arrays Probe", TinkerColor.random(), "🐄", NotePing) { (context, noteRef) =>
    implicit val probe: SpiritRef[Probe.Message] = Tinker.userExtension.probe
    implicit val tc: TinkerContext[?] = context

    noteRef.setMarkdown(s"Initializing with $valueList") match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }

    val zipped = valueList.zipWithIndex.map { case (int, startIndex) =>
      (startIndex, SelfSortingArrays.filename(startIndex, int), int)
    }

    val cells: NonEmptyList[CellWrapper[InsertionSortCell.Message]] = zipped.map {
      case (index, filename, value) =>
        val actorName = filename.replace(" ", "_").replace("(", "").replace(")", "")
        CellWrapper(
          index,
          value,
          filename,
          context.cast(
            InsertionSortCell(index, index, filename, value), actorName,
            DispatcherSelector.fromConfig("self-sorting-dispatcher") // use one thread
          )
        )
    }

    @tailrec
    def finishInitializingCells(remaining: List[Option[InsertionSortCellWrapper]]): Unit = {
      remaining match {
        case left :: Some(cell) :: right :: _ =>
          cell !!! Initialize(left, right)
          finishInitializingCells(remaining.tail)

        case List(_, _) => // done!

        case _ => ???
      }
    }
    finishInitializingCells(None :: cells.toList.map(Some(_)) ::: List(None))

    implicit val nr: NoteRef = noteRef
    println(s"click to sort $VALUE_LIST")
    noteRef.setMarkdown(s"- [ ] Check to sort $VALUE_LIST") match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }
    waiting(cells)
  }


  private def waiting(cells: NonEmptyList[CellWrapper[InsertionSortCell.Message]])(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef, probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case NotePing(_) =>
        if (noteRef.checkBoxIsChecked()) {
          println("Starting sort request!")
          cells.head !!! InsertionSortCell.DoSort
          val timeKeeper = context.castTimeKeeper()
          timeKeeper !! TimeKeeper.RemindMeEvery(2.seconds, 1.seconds, context.self, ClockTick, None)
          implicit val cellsList: List[CellWrapper[InsertionSortCell.Message]] = cells.toList
          clockTicking(0)
        } else {
          Tinker.steadily
        }

      case ClockTick => ???
    }
  }
  private def clockTicking(count: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef, cells: List[CellWrapper[InsertionSortCell.Message]], probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case NotePing(_) =>
        if (noteRef.checkBoxIsChecked()) {
          paused(count)
        } else {
          Tinker.steadily // ignore
        }
      case ClockTick =>
        noteRef.setMarkdown(s"- [ ] Pause\n\nsorting $VALUE_LIST\n\nclock tick: $count\n")
        Tinker.userExtension.probe !! Probe.ClockTick(count)
        for (cell <- cells) {
          cell !!! InsertionSortCell.ClockTick(count)
        }
        clockTicking(count + 1)
    }
  }

  private def paused(count: Int)(implicit Tinker: EnhancedTinker[SelfSortingArrayCentralCast], noteRef: NoteRef, cells: List[CellWrapper[InsertionSortCell.Message]], probe: SpiritRef[Probe.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case NotePing(_) =>
        if (!noteRef.checkBoxIsChecked()) {
          clockTicking(count)
        } else {
          Tinker.steadily // ignore
        }
      case ClockTick =>
        Tinker.steadily // ignore
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown()
        .map(markdown => {
          markdown.startsWith("- [x] ")
        }) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }

  //

  case class InvariantViolation(details: String) extends RuntimeException(details)
}


