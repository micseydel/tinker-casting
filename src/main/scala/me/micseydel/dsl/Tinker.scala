package me.micseydel.dsl

import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{Behavior, Scheduler}
import akka.util.Timeout
import me.micseydel.Common
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.vault.VaultKeeper
import me.micseydel.vault.VaultKeeper.{JsonRefResponse, NoteRefResponse}
import me.micseydel.vault.persistence._
import spray.json.JsonFormat

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

class Tinker(val tinkerSystem: TinkerSystem) {
  def setup[T](factory: TinkerContext[T] => Ability[T]): Ability[T] = Behaviors.setup { ctx =>
    factory(tinkerSystem.newContext(ctx))
  }

  def receive[T](onMessage: (TinkerContext[T], T) => Ability[T]): Ability[T] = Behaviors.receive { (ctx, message) =>
    onMessage(tinkerSystem.newContext(ctx), message)
  }

  def receiveMessage[T](onMessage: T => Ability[T]): Ability[T] = setup { _ =>
    Behaviors.receiveMessage(onMessage)
  }

  def steadily[T]: Ability[T] = Behaviors.same

  def done[T]: Ability[T] = Behaviors.stopped

  def empty[T]: Ability[T] = Behaviors.empty

  def unhandled[T]: Ability[T] = Behaviors.unhandled

  def ignore[T]: Ability[T] = Behaviors.ignore

}

object Tinker {
  type Ability[T] = Behavior[T]

  implicit class RichTinker(val tinker: Tinker) extends AnyVal {

    /**
     * message processing throws if appending to disk or fetching after fails
     */
    // FIXME: refactor
    def withPriorMessages[T](jsonlRef: JsonlRefT[T])(ability: (TinkerContext[T], T, List[T]) => Ability[T]): Ability[T] = tinker.setup { context =>
      tinker.receiveMessage[T] { message =>
        jsonlRef.appendAndGet(message) match {
          case Failure(exception) =>
            throw throw new RuntimeException(s"Failed to appendAndGet jsonl", exception)

          case Success(priorMessages) =>
            // FIXME: does re-using this context cause a problem?
            ability(context, message, priorMessages)
        }
      }
    }

    private[dsl] def withJson[T](jsonName: String)(f: JsonRef => Ability[T]): Ability[T] = tinker.setup { context =>
      implicit val scheduler: Scheduler = context.system.actorSystem.scheduler
      val duration = 1.seconds // FIXME: hopefully can be faster, or more likely, replaced
      implicit val timeout: Timeout = Timeout(duration)
      val jsonRef = Await.ready[JsonRefResponse](context.system.vaultKeeper.underlying.ask { replyTo =>
        VaultKeeper.RequestExclusiveJsonRef(jsonName, replyTo)
      }, duration).value match {
        case Some(Success(JsonRefResponse(responseJsonName, jsonRefOrWhyNot))) =>
          jsonRefOrWhyNot match {
            case Right(jsonRef) =>
              jsonRef
            case Left(whyNot) =>
              throw new RuntimeException(s"Failed to get JsonRef for $jsonName (responseJsonName=$responseJsonName): $whyNot")
          }

        case Some(Failure(throwable)) =>
          throw throwable
        case None =>
          throw new RuntimeException(s"Expected VaultKeeper.RequestExclusiveNoteRef ask to be non-empty")
      }

      f(jsonRef)
    }

    def withTypedJson[T, J](jsonName: String, jsonFormat: JsonFormat[J])(f: TypedJsonRef[J] => Ability[T]): Ability[T] = withJson(jsonName) { jsonJref =>
      f(new TypedJsonRef(jsonJref)(jsonFormat))
    }

    def withPersistedMessages[T, J](jsonName: String, jsonFormat: JsonFormat[J])(f: JsonlRef[J] => Ability[T]): Ability[T] = withJson(jsonName) { jsonJref =>
      f(new JsonlRef[J](jsonJref)(jsonFormat))
    }

    private def initializedWithNoteAndJsonPersistence[T](noteName: String, jsonName: String)(f: (TinkerContext[T], NoteRef, JsonRef) => Ability[T]): Ability[T] = {
      tinker.setup { context =>
        implicit val scheduler: Scheduler = context.system.actorSystem.scheduler
        implicit val duration: FiniteDuration = 1.seconds // FIXME: hopefully can be faster, or more likely, replaced
        implicit val timeout: Timeout = Timeout(duration)

        val noteFut: Future[NoteRefResponse] = context.system.vaultKeeper.underlying.ask { replyTo =>
          VaultKeeper.RequestExclusiveNoteRef(noteName, replyTo)
        }

        val jsonFut: Future[JsonRefResponse] = context.system.vaultKeeper.underlying.ask { replyTo =>
          VaultKeeper.RequestExclusiveJsonRef(jsonName, replyTo)
        }

        // FIXME: I believe the futures above run in parallel, document/cite

        val noteRef: NoteRef = Common.await[NoteRefResponse, NoteRef](noteFut, {
          case NoteRefResponse(responseNoteName, noteRefOrWhyNot) =>
            noteRefOrWhyNot match {
              case Right(noteRef) =>
                noteRef
              case Left(whyNot) =>
                throw new RuntimeException(s"Failed to get NoteRef for $noteName (responseNoteName=$responseNoteName): $whyNot")
            }
        })

        val jsonRef: JsonRef = Common.await[JsonRefResponse, JsonRef](jsonFut, {
          case JsonRefResponse(_, noteRefOrWhyNot) =>
            noteRefOrWhyNot match {
              case Right(jsonRef) =>
                jsonRef
              case Left(whyNot) =>
                throw new RuntimeException(s"Failed to get JsonRef for $jsonName ([[$noteName]]): $whyNot")
            }
        })

        f(context, noteRef, jsonRef)
      }
    }

    // FIXME: remove
    private[dsl] def initializedWithNoteAndPersistedMessages[T, J](noteName: String, jsonName: String, jsonFormat: JsonFormat[J])(f: (TinkerContext[T], NoteRef, JsonlRefT[J]) => Ability[T]): Ability[T] = {
      initializedWithNoteAndJsonPersistence(noteName, jsonName) { case (context, noteRef, jsonRef) =>
        val jsonlRef = new JsonlRef[J](jsonRef)(jsonFormat)
        f(context, noteRef, jsonlRef)
      }
    }

    private[dsl] def initializedWithNote[T](noteName: String, subdirectory: Option[String] = None)(f: (TinkerContext[T], NoteRef) => Ability[T]): Ability[T] = {
      tinker.setup { context =>
        implicit val scheduler: Scheduler = context.system.actorSystem.scheduler
        implicit val duration: FiniteDuration = 1.seconds // FIXME: hopefully can be faster, or more likely, replaced
        implicit val timeout: Timeout = Timeout(duration)

        val noteFut: Future[NoteRefResponse] = context.system.vaultKeeper.underlying.ask { replyTo =>
          VaultKeeper.RequestExclusiveNoteRef(noteName, replyTo, subdirectory)
        }

        val noteRef: NoteRef = Common.await[NoteRefResponse, NoteRef](noteFut, {
          case NoteRefResponse(responseNoteName, noteRefOrWhyNot) =>
            noteRefOrWhyNot match {
              case Right(noteRef) =>
                noteRef
              case Left(whyNot) =>
                throw new RuntimeException(s"Failed to get NoteRef for $noteName (responseNoteName=$responseNoteName): $whyNot")
            }
        })

        f(context, noteRef)
      }
    }
  }
}
