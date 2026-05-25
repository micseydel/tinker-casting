package me.micseydel.actor.tasks

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common
import me.micseydel.actor.notifications.NotificationCenterManager.NotificationCenterAbilities
import me.micseydel.actor.perimeter.NtfyerActor
import me.micseydel.app.AppConfiguration.AppConfig
import me.micseydel.app.{AppConfiguration, MyCentralCast}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.chronicler.Chronicler
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{EnhancedTinker, Tinker, TinkerColor, TinkerContainer, TinkerContext}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.VaultPath
import me.micseydel.vault.persistence.NoteRef

import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.unused
import scala.util.{Failure, Success, Try}

object RecurringResponsibilityManager {
  sealed trait Message

  final case class Track(wikilink: String, localDate: LocalDate) extends Message

  def apply()(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = NoteMakingTinkerer("Recurring responsibilities (MOC)", TinkerColor.random(), "⛑️") { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.readListOfWikiLinks() match {
      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"Something went wrong setting up ${noteRef.noteId}: $e")
        Tinker.ignore
      case Validated.Valid(recurringResponsibilities: NonEmptyList[Entry]) =>
        // FIXME: consider de-duping recurringResponsibilities, for when Obsidian Sync causes problems
        val specificResponsibilityTrackers = recurringResponsibilities.map { recurringResponsibility =>
          context.actorContext.log.info(s"Casting $recurringResponsibility")
          context.cast(RecurringResponsibilityActor(recurringResponsibility.wikilink, context.self), Common.tryToCleanForActorName(recurringResponsibility.wikilink))
        }

        behavior(recurringResponsibilities)
    }
  }

  private def behavior(tracking: NonEmptyList[Entry])(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.receive { (context, message) =>
    message match {
      case Track(wikilink, localDate) =>
        val now = context.system.clock.now()
        val filtered: List[Entry] = tracking.filter(_.wikilink != wikilink)
        val updatedState: NonEmptyList[Entry] = NonEmptyList(Entry(wikilink, Option(localDate)), filtered)
          .sortBy(_.maybeNextActionDate.map(_.toEpochSecond(now.toLocalTime, now.getOffset)).getOrElse(0L))

        noteRef.setMarkdown(updatedState.map {
          case Entry(wikilink, Some(nextActionDate)) =>
            s"[[$wikilink]] ($nextActionDate)"
          case Entry(wikilink, None) =>
            s"[[$wikilink]]"
        }.toList.mkString("- ", "\n- ", "\n"))

        behavior(updatedState)
    }
  }

  //

  private case class Entry(wikilink: String, maybeNextActionDate: Option[LocalDate])

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def readListOfWikiLinks(): ValidatedNel[String, NonEmptyList[Entry]] = {
      noteRef.readMarkdown().map(readListOfWikiLinks) match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(result) =>
          result
      }
    }

    private def readListOfWikiLinks(markdown: String): ValidatedNel[String, NonEmptyList[Entry]] = {
      val lines = markdown.split("\n")

      val (problems: List[String], parsed: List[Entry]) = lines.toList.zipWithIndex.partitionMap {
        case (line, i) =>
          if (line.startsWith("- [[") && line.endsWith("]]")) {
            Right(Entry(line.drop(4).dropRight(2), None))
          } else if (line.startsWith("- [[") && line.contains("]]")) {
            line.split("]] ").toList match {
              case List(wikiLinkPlusPrefixCruft, almostLocalDate) =>
                Try(LocalDate.parse(almostLocalDate.drop(1).dropRight(1))) match {
                  case Failure(exception) =>
                    Left(Common.getStackTraceString(exception))
                  case Success(localDate) =>
                    Right(Entry(wikiLinkPlusPrefixCruft.drop(4), Some(localDate)))
                }

              case other =>
                Left(s"Unrecognized format on line $i: $other")
            }
          } else {
            Left(s"Unrecognized format on line $i: $line")
          }
      }

      problems match {
        case head :: tail =>
          NonEmptyList(head, tail).invalid
        case Nil =>
          parsed match {
            case head :: tail =>
              NonEmptyList(head, tail).valid
            case Nil =>
              "No lines found".invalidNel
          }
      }
    }
  }
}


object RecurringResponsibilityManagerDemo {
  def main(args: Array[String]): Unit = {
    AppConfiguration.getConfig() match {
      case Validated.Invalid(errors) =>
        println(s"FAILED, errors-\n$errors")
      case Validated.Valid(config: AppConfig) =>
        println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Starting system: config with vault root ${config.vaultRoot}, creating json/ subdirectory if needed")
        run(config)
    }
  }

  def run(config: AppConfig): Unit = {
    @unused
    val container =
      TinkerContainer(config, NotificationCenterAbilities.None.copy(ntfy = NtfyerActor()(_)), "RecurringResponsibilityManagerDemo")(
        centralCastFactory(config.vaultRoot)(_, _),
        RecurringResponsibilityManager()(_: EnhancedTinker[MyCentralCast])
      )

    println(s"[${TimeUtil.zonedDateTimeToISO8601(ZonedDateTime.now())}] Demo system done starting")
  }

  private def centralCastFactory(vaultRoot: VaultPath)(implicit Tinker: Tinker, context: TinkerContext[?]): MyCentralCast = {
    context.actorContext.log.info("Creating central cast with Chronicler, Gossiper and INERT Rasa")
    val gossiper = context.cast(Gossiper(), "Gossiper") // FIXME: this has not been tested, relies on mqtt which I have not yet configured for this testing
    val chronicler = context.cast(Chronicler(vaultRoot, gossiper), "Chronicler")
    val rasaActor = context.cast(InertActor(), "INERTRasaActorFORTESTING")
    MyCentralCast(chronicler, gossiper, rasaActor)
  }
}

// FIXME: consolidate with the one in LitterBoxesHelper
object InertActor {
  def apply[T](): Behavior[T] = Behaviors.receive { case (context, message) =>
    context.log.debug(s"ignoring $message")
    Behaviors.same
  }
}
