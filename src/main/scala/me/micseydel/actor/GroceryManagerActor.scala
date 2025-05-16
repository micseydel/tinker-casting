package me.micseydel.actor

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.GmailActor.Email
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.util.MarkdownUtil
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}

import java.io.FileNotFoundException
import java.time.{LocalDate, ZoneId, ZonedDateTime}
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
          context.cast(GroceryListMOCActor(noteName), Common.tryToCleanForActorName(noteName))
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
        // FIXME: https://github.com/micseydel/tinker-casting/issues/21 documents the need to verify sender
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
    noteRef.readValidatedDocument() match {
      case Validated.Valid(Document(nextNote, _, _, Config(senderEquals, subjectContains))) =>
        implicit val currentGroceryNoteActor: SpiritRef[CurrentGroceryNoteActor.Message] = context.cast(CurrentGroceryNoteActor(nextNote), Common.tryToCleanForActorName(nextNote))
        implicit val doTurnOverFor: (Seq[Email], ZonedDateTime) => Option[LocalDate] = anEmailIndicatesTurnOver(senderEquals, subjectContains)(_, _)
        behavior(Map.empty)

      case Validated.Invalid(problems) =>
        context.actorContext.log.warn(s"Something(s) went wrong: $problems")
        Tinker.ignore
    }
  }

  private def behavior(archivalSpiritRefs: Map[LocalDate, SpiritRef[ArchivalGroceryNoteActor.Message]])(implicit Tinker: Tinker, noteRef: NoteRef, currentGroceryNoteActor: SpiritRef[CurrentGroceryNoteActor.Message], doTurnOverFor: (Seq[Email], ZonedDateTime) => Option[LocalDate]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context
    Tinker.receiveMessage {
      case ReceiveEmails(emails) =>
        noteRef.readValidatedDocument() match {
          case Validated.Valid(document: Document) =>
            val latestArchiveNote = document.latestArchive
            val latestDate = LocalDate.parse(latestArchiveNote.dropRight(1).takeRight(10)).atStartOfDay(ZoneId.systemDefault()) // by convention 😬
            context.actorContext.log.info(s"Received ${emails.size} emails, read wikilinks list from [[${noteRef.noteId}]] with latestArchiveNote [[$latestArchiveNote]], latestDate $latestDate")

            doTurnOverFor(emails, latestDate) match {
              case Some(day) =>
                context.actorContext.log.info(s"Turn over detected after $latestDate, for $day")
                archivalSpiritRefs.get(day) match {
                  case Some(existing) =>
                    context.actorContext.log.warn(s"$day was already created, which is a little surprise")
                    currentGroceryNoteActor !! CurrentGroceryNoteActor.DoTurnOver(existing)
                    Tinker.steadily
                  case None =>
                    // this is just to keep whatever the convention happened ot be
                    val newNoteName = latestArchiveNote.replace(latestDate.toString.take(10), day.toString)
                    if (document.latestArchive == newNoteName) {
                      context.actorContext.log.warn(s"(make this info) Not turning over note because it was already done [[${document.latestArchive}]]")
                      Tinker.steadily
                    } else {
                      context.actorContext.log.info(s"Creating SpiritRef for $newNoteName")
                      val newArchivalNote = context.cast(ArchivalGroceryNoteActor(newNoteName), Common.tryToCleanForActorName(newNoteName))
                      currentGroceryNoteActor !! CurrentGroceryNoteActor.DoTurnOver(newArchivalNote)
                      noteRef.setMarkdown(document.withNewLatest(newNoteName).toMarkdown)
                      behavior(archivalSpiritRefs.updated(day, newArchivalNote))
                    }
                }

              case None =>
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

  private def anEmailIndicatesTurnOver(senderEquals: String, subjectContains: String)(emails: Seq[Email], lastSeenDate: ZonedDateTime): Option[LocalDate] = {
    emails.flatMap {
      case email@Email(sender, subject, _, _, _) =>
        email.getTimeHacky match {
          case Success(dateFromEmail) =>
            val matches = dateFromEmail.isAfter(lastSeenDate) && sender == senderEquals && subject.contains(subjectContains)
            if (matches) {
              Some(dateFromEmail.toLocalDate)
            } else {
              None
            }
          case Failure(exception) => throw exception
        }
    }
  }.maxOption

  //

  case class Config(senderEquals: String, subjectContains: String)

  case class Document(nextNote: String, latestArchive: String, older: List[String], config: Config) {
    def withNewLatest(latest: String): Document = {
      if (latest == latestArchive) {
        throw new RuntimeException(s"Received request to add [[$latest]] but that was already the latest")
      }
      Document(nextNote, latest, latestArchive :: older, config)
    }

    def toMarkdown: String = {
      val newLines = (nextNote :: latestArchive :: older).map(link => s"- [[$link]]")
      newLines.mkString("\n")
    }
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def readValidatedDocument(): ValidatedNel[String, Document] = {
      noteRef.readNote() match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(note@Note(markdown, _)) =>
          note.yamlFrontMatter match {
            case Failure(exception) =>
              s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
            case Success(map) =>
              val validatedConfig = getConfigFromMap(map)
              val validatedItems = MarkdownUtil.readListOfWikiLinks(markdown)

              validatedConfig.andThen { config =>
                validatedItems.map(_.toList).andThen {
                  case next :: latest :: older =>
                    Document(next, latest, older, config).validNel
                  case other =>
                    s"Expected at least two items but got $other".invalidNel
                }
              }
          }
      }
    }

    private def getConfigFromMap(map: Map[String, Any]): ValidatedNel[String, Config] = {
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
}

object CurrentGroceryNoteActor {
  sealed trait Message

  final case class DoTurnOver(archivalNote: SpiritRef[ArchivalGroceryNoteActor.Message]) extends Message

  def apply(noteName: String)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer(noteName, TinkerColor.random(), "☑️️") { (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context
    Tinker.receiveMessage {
      case DoTurnOver(archivalNote) =>
        noteRef.readMarkdown().map { markdown =>
          val (turningOver, keeping) = markdown.split("\n").partition(_.startsWith("- [x] "))
          if (turningOver.isEmpty) {
            context.actorContext.log.warn(s"Got a turn over request for $archivalNote but no checkboxes were marked")
          } else {
            context.actorContext.log.info(s"Turning over ${turningOver.length} items")
            // this should be a transaction https://github.com/micseydel/tinker-casting/issues/22
            noteRef.setMarkdown(keeping.mkString("\n"))
            archivalNote !! ArchivalGroceryNoteActor.AddContents(turningOver.toList)
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
        val toAppend = lines.mkString("\n")
        noteRef.readMarkdown().flatMap { markdown =>
          if (markdown.contains(toAppend)) {
            context.actorContext.log.warn(s"Ignoring duplicate request to add ${lines.size} to [[$noteName]]")
            Success(NoOp)
          } else {
            context.actorContext.log.info(s"Adding ${lines.size} to [[$noteName]]")
            noteRef.append(toAppend)
          }
        } match {
          case Failure(_: FileNotFoundException) =>
            noteRef.setMarkdown(toAppend) match {
              case Failure(exception) => throw exception
              case Success(_) =>
            }
          case Failure(exception) => throw exception
          case Success(_) =>
        }
        Tinker.steadily
    }
  }
}
