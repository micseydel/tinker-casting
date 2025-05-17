package me.micseydel.actor

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.GmailActor.Email
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl._
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}

import java.time.LocalDate
import scala.util.{Failure, Success}

object GroceryManagerActor {
  sealed trait Message

  private final case class ReceiveEmails(emails: Seq[Email]) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message]("Grocery Lists", TinkerColor.random(), "🛒") { case (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context

    noteRef.readListOfWikiLinks() match {
      case Validated.Valid(groceryLists: NonEmptyList[String]) =>
        context.actorContext.log.info(s"Initializing with grocery lists $groceryLists, subscribing to Gmail")
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
        context.actorContext.log.info(s"Received ${emails.size} emails, sending to ${specificStores.size} grocery lists")
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
        // FIXME: do a SINGLE read from the noteref
        noteRef.readListOfWikiLinks() match {
          case Validated.Valid(wikilinks: NonEmptyList[String]) =>
            implicit val currentGroceryNoteActor: SpiritRef[CurrentGroceryNoteActor.Message] = context.cast(CurrentGroceryNoteActor(wikilinks.head), wikilinks.head.replace(" ", "_").replace("'", ""))
            implicit val doTurnOver: (Seq[Email], LocalDate) => Boolean = anEmailIndicatesTurnOver(senderEquals, subjectContains)(_, _)
            behavior(Map.empty)
          case Validated.Invalid(problems) =>
            context.actorContext.log.warn(s"Something(s) went wrong getting wikilinks: $problems")
            Tinker.ignore
        }

      case Validated.Invalid(problems) =>
        context.actorContext.log.warn(s"Something(s) went wrong getting config: $problems")
        Tinker.ignore
    }
  }

  private def behavior(archivalSpiritRefs: Map[LocalDate, SpiritRef[ArchivalGroceryNoteActor.Message]])(implicit Tinker: Tinker, noteRef: NoteRef, currentGroceryNoteActor: SpiritRef[CurrentGroceryNoteActor.Message], doTurnOver: (Seq[Email], LocalDate) => Boolean): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context
    Tinker.receiveMessage {
      case ReceiveEmails(emails) =>
        context.actorContext.log.info(s"Received ${emails.size} emails, reading wikilinks list from ${noteRef.noteId}")
        noteRef.readListOfWikiLinks() match {
          case Validated.Valid(wikilinks: NonEmptyList[String]) =>
            // assumes the first wikilink is the "active" [[Next ...]] link, and the second one is the last archival one
            val latestArchiveNote = wikilinks.tail.head
            val latestDate = LocalDate.parse(latestArchiveNote.dropRight(1).takeRight(10)) // by convention 😬
            if (doTurnOver(emails, latestDate)) {
              context.actorContext.log.info(s"Turn over detected after $latestDate")
              // FIXME: today is a hack, this should be re-worked so that it doesn't behave improperly if the email arrives after midnight
              val today = context.system.clock.today()
              archivalSpiritRefs.get(today) match {
                case Some(existing) =>
                  context.actorContext.log.warn(s"$today was already created, which is a little surprise")
                  currentGroceryNoteActor !! CurrentGroceryNoteActor.DoTurnOver(existing)
                  Tinker.steadily
                case None =>
                  val newNoteName = latestArchiveNote.replace(latestDate.toString, today.toString)
                  val newArchivalNote = context.cast(ArchivalGroceryNoteActor(newNoteName), newNoteName.replace(" ", "_").replace("'", ""))
                  context.actorContext.log.info(s"Creating $newNoteName")
                  currentGroceryNoteActor !! CurrentGroceryNoteActor.DoTurnOver(newArchivalNote)
                  noteRef.setMarkdown((wikilinks.head :: newNoteName :: wikilinks.tail).mkString("\n"))
                  behavior(archivalSpiritRefs.updated(today, newArchivalNote))
              }
            } else {
              context.actorContext.log.info(s"Receive ${emails.size} but none were a match for groceries")
              Tinker.steadily
            }
          case Validated.Invalid(problems) =>
            context.actorContext.log.warn(s"Received email(s), expected ${noteRef.noteId} to contain a list of wikilinks but: $problems")
            Tinker.steadily
        }
    }
  }

  //

  private def anEmailIndicatesTurnOver(senderEquals: String, subjectContains: String)(emails: Seq[Email], lastSeenDate: LocalDate): Boolean = {
    emails.exists {
      case Email(sender, subject, _, sentAt, _) =>
        sender == senderEquals && subject.contains(subjectContains) &&
          sentAt.endsWith("PDT") && sentAt.contains(lastSeenDate.toString)
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

object CurrentGroceryNoteActor {
  sealed trait Message

  final case class DoTurnOver(archivalNote: SpiritRef[ArchivalGroceryNoteActor.Message]) extends Message

  def apply(noteName: String)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "☑️️") { (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context
    // FIXME: create a listener :)
    Tinker.receiveMessage {
      case DoTurnOver(archivalNote) =>
        noteRef.readMarkdown().map { markdown =>
          val (turningOver, keeping) = markdown.split("\n").partition(_.startsWith("- [x] "))
          if (turningOver.isEmpty) {
            context.actorContext.log.warn(s"Got a turn over request for $archivalNote but no checkboxes were marked")
          } else {
            context.actorContext.log.info(s"Turning over ${turningOver.length} items")
            archivalNote !! ArchivalGroceryNoteActor.AddContents(turningOver.toList)
            noteRef.setMarkdown(keeping.mkString("\n"))
          }
        }

        Tinker.steadily
    }
  }
}

object ArchivalGroceryNoteActor {
  sealed trait Message

  final case class AddContents(lines: Seq[String]) extends Message

  def apply(noteName: String)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "✅️") { (context, noteRef) =>
    Tinker.receiveMessage {
      case AddContents(lines) =>
        noteRef.readMarkdown().flatMap { markdown =>
          val toAppend = lines.mkString("\n")
          if (markdown.contains(toAppend)) {
            context.actorContext.log.warn(s"Ignoring duplicate request to add ${lines.size} to [[$noteName]]")
            Success(NoOp)
          } else {
            context.actorContext.log.info(s"Adding ${lines.size} to [[$noteName]]")
            noteRef.append(toAppend)
          }
        }
        Tinker.steadily
    }
  }
}
