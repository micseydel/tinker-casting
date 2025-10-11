package me.micseydel.dsl.cast.chronicler

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId

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

  def apply()(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Starting ChroniclerMOC")
    behavior(Map.empty)
  }

  private def behavior(noteKeepers: Map[LocalDate, SpiritRef[ChroniclerMOCDailyNote.PostInitMessage]])(implicit Tinker: Tinker): Ability[Message] =
    Tinker.receive { (context, incomingMessage) =>
      val day = incomingMessage.time.toLocalDate

      val outgoingMessage: ChroniclerMOCDailyNote.PostInitMessage = incomingMessage match {
        case AddNote(noteEntry) =>
          context.actorContext.log.info(s"Forwarding AddNote ${noteEntry.ref}")
          ChroniclerMOCDailyNote.AddNote(noteEntry)
        case ListenerAcknowledgement(noteRef, timeOfAck, details, setState) =>
          context.actorContext.log.info(s"Forwarding ListenerAcknowledgement $noteRef")
          ChroniclerMOCDailyNote.ListenerAcknowledgement(noteRef, timeOfAck, details, setState)
      }

      implicit val c: TinkerContext[_] = context
      noteKeepers.get(day) match {
        case Some(chroniclerDailyMOC) =>
          chroniclerDailyMOC !! outgoingMessage
          Tinker.steadily

        case None =>
          val freshChroniclerDailyMOC = context.cast(ChroniclerMOCDailyNote(day), TimeUtil.localDateToISO8601Date(day))
          freshChroniclerDailyMOC !! outgoingMessage
          behavior(noteKeepers.updated(day, freshChroniclerDailyMOC))
      }
    }

  final case class TranscribedMobileNoteEntry(time: ZonedDateTime, ref: NoteId, wordCount: Int)
}
