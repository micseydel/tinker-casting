package me.micseydel.dsl.cast.chronicler

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.util.TimeUtil
import me.micseydel.vault.{NoteId, VaultKeeper}

import java.nio.file.Path
import java.time.{LocalDate, ZonedDateTime}

object ChroniclerMOC {

  sealed trait Message {
    def time: ZonedDateTime

    //    def key: String
  }

  /**
   * Idempotent - keyed off of noteEntry.time
   */
  case class AddNote(noteEntry: TranscribedMobileNoteEntry) extends Message {
    override def time: ZonedDateTime = noteEntry.time

    //    override def key: String = time.toString // Common.zonedDateTimeToISO8601Date(time)
  }

  case class ListenerAcknowledgement(noteRef: NoteId, timeOfAck: ZonedDateTime, details: String, setState: Option[NoteState]) extends Message {
    override def time: ZonedDateTime = timeOfAck

    //    override def key: String = s"${Common.zonedDateTimeToISO8601Date(time)}"
  }

  // note state

  sealed trait NoteState

  case object NeedsAttention extends NoteState

  case object AutomaticallyIntegrated extends NoteState

  // behavior

  def apply(jsonPath: Path, vaultKeeper: ActorRef[VaultKeeper.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Starting ChroniclerMOC with jsonPath $jsonPath")
    // FIXME: replace this map with DailyRouter or whatever?
    behavior(jsonPath, vaultKeeper, Map.empty)
  }

  private def behavior(jsonPath: Path, vaultKeeper: ActorRef[VaultKeeper.Message], noteKeepers: Map[LocalDate, SpiritRef[ChroniclerMOCDailyNote.PostInitMessage]])(implicit Tinker: Tinker): Ability[Message] =
    Tinker.receive { (context, incomingMessage) =>
      val day = incomingMessage.time.toLocalDate

      val outgoingMessage: ChroniclerMOCDailyNote.PostInitMessage = incomingMessage match {
        case AddNote(noteEntry) =>
          context.actorContext.log.info(s"Forwarding AddNote ${noteEntry.ref}")
          ChroniclerMOCDailyNote.AddNote(noteEntry)
        case ListenerAcknowledgement(noteRef, timeOfAck, details, setState) =>
          // FIXME: this should be idempotent
          context.actorContext.log.info(s"Forwarding ListenerAcknowledgement $noteRef")
          ChroniclerMOCDailyNote.ListenerAcknowledgement(noteRef, timeOfAck, details, setState)
      }

      implicit val c: TinkerContext[_] = context
      noteKeepers.get(day) match {
        case Some(chroniclerDailyMOC) =>
          chroniclerDailyMOC !! outgoingMessage
          Behaviors.same

        case None =>
          val freshChroniclerDailyMOC = context.cast(ChroniclerMOCDailyNote(day), TimeUtil.localDateToISO8601Date(day))
          freshChroniclerDailyMOC !! outgoingMessage
          behavior(jsonPath, vaultKeeper, noteKeepers.updated(day, freshChroniclerDailyMOC))
      }
    }

  final case class TranscribedMobileNoteEntry(time: ZonedDateTime, ref: NoteId, wordCount: Int)
}
