package me.micseydel.actor.kitties

import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.kitties.LitterPipelineExperiment.StateJsonProtocol.eventFormat
import me.micseydel.dsl.Tinker
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.CatBrown
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import spray.json._

import java.io.FileNotFoundException
import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object LitterPipelineExperiment {
  sealed trait Message

  private case class ReceiveNotePing(ping: Ping) extends Message
  case class ReceiveNote(forDay: LocalDate, markdown: String) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceiveNotePing](
    "LitterPipelineExperiment", CatBrown, "🪈", ReceiveNotePing) { case (context, noteRef) =>
    //
    Tinker.receiveMessage {
      case ReceiveNotePing(_) =>
        context.actorContext.log.warn(s"checkBoxIsChecked? ${noteRef.checkBoxIsChecked()}")
        Tinker.steadily

      case ReceiveNote(forDay, markdown) =>
        context.actorContext.log.debug(s"Saving state for day $forDay, ${markdown.length} characters of mMarkdown")
        noteRef.saveState(forDay, markdown) match {
          case Failure(exception) => context.actorContext.log.warn(s"Something went wrong saving state for date $forDay", exception)
          case Success(_) =>
        }
        Tinker.steadily
    }
  }

  //

  case class State(map: Map[LocalDate, String])

  object StateJsonProtocol extends DefaultJsonProtocol {
    implicit object LocalDateTypeJsonFormat extends RootJsonFormat[LocalDate] {
      def write(e: LocalDate): JsString = JsString(e.toString)

      def read(value: JsValue): LocalDate = value match {
        case JsString(s) => LocalDate.parse(s)
        case _ => throw DeserializationException(s"An ISO local date")
      }
    }

    implicit val stateMapFormat: RootJsonFormat[Map[LocalDate, String]] = mapFormat[LocalDate, String]
    implicit val eventFormat: RootJsonFormat[State] = jsonFormat1(State)
  }

  //

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def saveState(forDay: LocalDate, markdownForDay: String): Try[Note] = {
      noteRef.readNote().recoverWith {
        case _: FileNotFoundException =>
          Success(Note("", None))
        case other => Failure(other)
      }.flatMap {
        case Note(pipelineMarkdown, maybeFrontmatter) =>
          val updatedFrontmatter = maybeFrontmatter
            .map(_.parseJson.convertTo[State])
            .map(_.map)
            .orElse(Some(Map.empty[LocalDate, String]))
            .map(_.updated(forDay, markdownForDay))
            .map(State)
            .map(_.toJson.toString())
          noteRef.setTo(Note(pipelineMarkdown, updatedFrontmatter))
      }
    }

    def checkBoxIsChecked(): Boolean =
      noteRef.readMarkdown()
        .map(markdown => markdown.startsWith("- [x] ")) match {
        case Failure(exception) => throw exception
        case Success(result) => result
      }
  }
}
