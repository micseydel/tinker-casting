package me.micseydel.actor

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.GmailActor.Email
import me.micseydel.dsl.{Operator, SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault.persistence.NoteRef

import java.time.LocalDate
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

object GroceryManagerActor {
  sealed trait Message

  final case class ReceiveEmails(emails: Seq[Email]) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message]("Grocery Lists", TinkerColor.random(), "🛒") { case (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context

    noteRef.readListOfWikiLinks() match {
      case Validated.Valid(groceryLists: NonEmptyList[String]) =>
        val specificStores: NonEmptyList[SpiritRef[GroceryListMOCActor.Message]] = groceryLists.map { noteName =>
          context.cast(GroceryListMOCActor(noteName), noteName.replace(" ", "_").replace("'", ""))
        }

        context.system.operator !! Operator.SubscribeGmail(context.messageAdapter(ReceiveEmails))
        behavior(specificStores.toList)

      case Validated.Invalid(problems: NonEmptyList[String]) =>
        context.actorContext.log.warn(s"Ran into (a) problem(s) on startup: $problems")
        Tinker.ignore
    }
  }

  private def behavior(specificStores: List[SpiritRef[GroceryListMOCActor.Message]])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context
    Tinker.receiveMessage {
      case ReceiveEmails(emails) =>
        // FIXME: oof https://dkim.org/specs/rfc4871-dkimbase.html#dkim-sig-hdr
        //   maybe do this in GmailActor
        specificStores.foreach(_ !! GroceryListMOCActor.ReceiveEmails(emails))
        Tinker.steadily
    }
  }

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def readListOfWikiLinks(): ValidatedNel[String, NonEmptyList[String]] = {
      noteRef.readMarkdown().map(MarkdownUtil.readListOfWikiLinks) match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(result) =>
          result
      }
    }
  }
}

object GroceryListMOCActor {
  sealed trait Message

  final case class ReceiveEmails(emails: Seq[Email]) extends Message

  def apply(noteName: String)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "🛒") { (context, noteRef) =>
    implicit val nr: NoteRef = noteRef
    noteRef.getConfig() match {
      case Validated.Valid(Config(senderEquals, subjectContains)) =>
        behavior(senderEquals, subjectContains)
      case Validated.Invalid(e) =>
        context.actorContext.log.warn(s"Something(s) went wrong: $e")
        Tinker.ignore
    }
  }

  private def behavior(senderEquals: String, subjectContains: String)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    def anEmailIndicatesTurnOver(emails: Seq[Email]): Boolean = {
      val today = context.system.clock.today() // FIXME: this also depends on the latest note
      emails.exists {
        case Email(sender, subject, _, sentAt, groupedHeaders) =>
          sender == senderEquals && subject.contains(subjectContains) &&
          sentAt.endsWith("PDT") && sentAt.contains(today.toString)
      }
    }

    Tinker.receiveMessage {
      case ReceiveEmails(emails) =>
        val doTurnOver = anEmailIndicatesTurnOver(emails)

        if (doTurnOver) {
          // FIXME: document any assumptions!
          // FIXME: I was taking for granted that Obsidian renames links!
          context.actorContext.log.warn(s"The [[Next...]] note needs to be renamed based on the email date, and a new one created; ideally the non-checked items are carried over")

          noteRef.readListOfWikiLinks() match {
            case Validated.Valid(wikilinks) => ???
            case Validated.Invalid(problems) => ???
          }

        } else {
          context.actorContext.log.info(s"Receive ${emails.size} but none were a match for groceries")
        }

        Tinker.steadily
    }
  }

  //

  private case class Config(senderEquals: String, subjectContains: String)

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def getConfig(): ValidatedNel[String, Config] = {
      noteRef.readNote().flatMap(_.yamlFrontMatter) match {
        case Failure(exception) => Common.getStackTraceString(exception).invalidNel
        case Success(map) =>
          (map.get("sender_equals"), map.get("subject_contains")) match {
            case (Some(sender_equals: String), Some(subject_contains: String)) =>
              Config(sender_equals, subject_contains).validNel

            case (None, None) =>
              "Expected properties: sender_equals and subject_contains".invalidNel

            case other =>
              s"Expected properties (Some(sender_equals: String), Some(subject_contains: String)) but got $other".invalidNel
          }
      }
    }

    def readListOfWikiLinks(): ValidatedNel[String, NonEmptyList[String]] = {
      noteRef.readMarkdown().map(MarkdownUtil.readListOfWikiLinks) match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(result) => result
      }
    }
  }
}

