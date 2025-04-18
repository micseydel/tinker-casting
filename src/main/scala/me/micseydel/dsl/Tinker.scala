package me.micseydel.dsl

import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{Behavior, Scheduler}
import akka.util.Timeout
import me.micseydel.actor.ActorNotesFolderWatcherActor
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.{Ability, await}
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.TinkerBrain.RegisterTinkerer
import me.micseydel.dsl.cast.chronicler.Chronicler.ListenerAcknowledgement
import me.micseydel.model.NotedTranscription
import me.micseydel.vault.VaultKeeper
import me.micseydel.vault.VaultKeeper.{JsonRefResponse, NoteRefResponse}
import me.micseydel.vault.persistence.{TypedJsonRef, _}
import spray.json.JsonFormat

import java.net.{URI, URLEncoder}
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}

class Tinker(val tinkerSystem: TinkerSystem) {
  def setup[T](factory: TinkerContext[T] => Ability[T]): Ability[T] = Behaviors.setup { ctx =>
    factory(tinkerSystem.newContext(ctx))
  }

  def receive[T](onMessage: (TinkerContext[T], T) => Ability[T]): Ability[T] = Behaviors.receive { (ctx, message) =>
    onMessage(tinkerSystem.newContext(ctx), message)
  }

  private[dsl] def withJson[T](jsonName: String)(f: JsonRef => Ability[T]): Ability[T] = setup { context =>
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

  def withTypedJson[T, J](jsonName: String, jsonFormat: JsonFormat[J])(f: (TypedJsonRef[J]) => Ability[T]): Ability[T] = withJson(jsonName) { jsonJref =>
    f(new TypedJsonRef(jsonJref)(jsonFormat))
  }

  def withPersistedMessages[T, J](jsonName: String, jsonFormat: JsonFormat[J])(f: JsonlRef[J] => Ability[T]): Ability[T] = withJson(jsonName) { jsonJref =>
    f(new JsonlRef[J](jsonJref)(jsonFormat))
  }

  private def initializedWithNoteAndJsonPersistence[T](noteName: String, jsonName: String)(f: (TinkerContext[T], NoteRef, JsonRef) => Ability[T]): Ability[T] = {
    setup { context =>
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

      val noteRef: NoteRef = await[NoteRefResponse, NoteRef](noteFut, {
        case NoteRefResponse(responseNoteName, noteRefOrWhyNot) =>
          noteRefOrWhyNot match {
            case Right(noteRef) =>
              noteRef
            case Left(whyNot) =>
              throw new RuntimeException(s"Failed to get NoteRef for $noteName (responseNoteName=$responseNoteName): $whyNot")
          }
      })

      val jsonRef: JsonRef = await[JsonRefResponse, JsonRef](jsonFut, {
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
    setup { context =>
      implicit val scheduler: Scheduler = context.system.actorSystem.scheduler
      implicit val duration: FiniteDuration = 1.seconds // FIXME: hopefully can be faster, or more likely, replaced
      implicit val timeout: Timeout = Timeout(duration)

      val noteFut: Future[NoteRefResponse] = context.system.vaultKeeper.underlying.ask { replyTo =>
        VaultKeeper.RequestExclusiveNoteRef(noteName, replyTo, subdirectory)
      }

      val noteRef: NoteRef = await[NoteRefResponse, NoteRef](noteFut, {
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

  def receiveMessage[T](onMessage: T => Ability[T]): Ability[T] = setup { _ =>
    Behaviors.receiveMessage(onMessage)
  }

  def steadily[T]: Ability[T] = Behaviors.same

  def done[T]: Ability[T] = Behaviors.stopped

  def empty[T]: Ability[T] = Behaviors.empty

  def unhandled[T]: Ability[T] = Behaviors.unhandled

  def ignore[T]: Ability[T] = Behaviors.ignore

  /**
   * message processing throws if appending to disk or fetching after fails
   */
    // FIXME: refactor
  def withPriorMessages[T](jsonlRef: JsonlRefT[T])(ability: (TinkerContext[T], T, List[T]) => Ability[T]): Ability[T] = setup { context =>
    receiveMessage[T] { message =>
      jsonlRef.appendAndGet(message) match {
        case Failure(exception) =>
          throw throw new RuntimeException(s"Failed to appendAndGet jsonl", exception)

        case Success(priorMessages) =>
          // FIXME: does re-using this context cause a problem?
          ability(context, message, priorMessages)
      }
    }
  }

  def withWatchedActorNote[T](noteName: String, adapterF: Ping => T)(f: (TinkerContext[T], NoteRef) => Ability[T]): Ability[T] =
    initializedWithNote(noteName, Some("_actor_notes")) { (context, noteRef) =>
      implicit val c: TinkerContext[_] = context
      context.system.actorNotesFolderWatcherActor !! ActorNotesFolderWatcherActor.SubscribeNoteRef(noteRef, context.messageAdapter(adapterF))
      f(context, noteRef)
    }
}

object Tinker {
  type Ability[T] = Behavior[T]

  def await[FutureResult, FinalResult](fut: Future[FutureResult], onSuccess: FutureResult => FinalResult)(implicit duration: FiniteDuration): FinalResult = {
    Await.ready(fut, duration).value match {
      case Some(Success(futureResult)) =>
        onSuccess(futureResult)

      case Some(Failure(throwable)) =>
        throw throwable
      case None =>
        throw new RuntimeException(s"Expected ask to be non-empty")
    }
  }
}

trait TinkerClock {
  def now(): ZonedDateTime

  def now(zoneId: ZoneId): ZonedDateTime

  def today(): LocalDate
}

class TinkerClockImpl extends TinkerClock {
  def now(): ZonedDateTime = ZonedDateTime.now()

  def now(zoneId: ZoneId): ZonedDateTime = ZonedDateTime.now(zoneId)

  def today(): LocalDate = LocalDate.now()
}


case class Tinkerer[T](color: TinkerColor, emoji: String, href: Option[String] = None) {
  def setup(factory: TinkerContext[T] => Ability[T])(implicit Tinker: Tinker): Ability[T] = Tinker.setup { context =>
    val registering = RegisterTinkerer(context.self.path, this)
    // FIXME: investigate this further, but this line seems to cause a CACHED logger where the calling class
    //   ends up as "me.micseydel.dsl.Tinkerer" which messages with custom logback stuff
    //    context.actorContext.log.info(s"registering $registering")

    context.system.tinkerBrain ! registering

    factory(context)
  }

  def receive(onMessage: (TinkerContext[T], T) => Ability[T])(implicit Tinker: Tinker): Ability[T] =
    setup(_ => Tinker.receive(onMessage))

  private[dsl] def withNote(noteName: String, subdirectory: Option[String] = None)(f: (TinkerContext[T], NoteRef) => Ability[T])(implicit Tinker: Tinker): Ability[T] = {
    setup(_ => Tinker.initializedWithNote(noteName, subdirectory)(f))
  }

  def initializedWithTypedJson[J](jsonName: String, jsonFormat: JsonFormat[J])(f: (TinkerContext[T], TypedJsonRef[J]) => Ability[T])(implicit Tinker: Tinker): Ability[T] = {
    setup(context => Tinker.withTypedJson(jsonName, jsonFormat)(f(context, _)))
  }

  def initializedWithPersistedMessages[J](jsonName: String, jsonFormat: JsonFormat[J])(f: (TinkerContext[T], JsonlRef[J]) => Ability[T])(implicit Tinker: Tinker): Ability[T] = {
    setup(context => Tinker.withPersistedMessages(jsonName, jsonFormat)(f(context, _)))
  }
}

case class TinkerColor(r: Int, g: Int, b: Int, o: Double = 1.0) {
  override def toString: String = if (o == 1.0) {
    s"rgb($r, $g, $b)"
  } else {
    s"rgba($r, $g, $b, $o)"
  }
}

object TinkerColor {
  val Yellow: TinkerColor = rgb(255, 255, 0)
  val CatBrown: TinkerColor = rgb(140, 100, 90)
  val Purple: TinkerColor = rgb(102, 15, 213)

  //

  def random(r: Option[Int] = None, g: Option[Int] = None, b: Option[Int] = None, o: Double = 1.0): TinkerColor = {
    TinkerColor(r.getOrElse(Random.nextInt(256)), g.getOrElse(Random.nextInt(256)), b.getOrElse(Random.nextInt(256)), o)
  }

  def rgb(r: Int, g: Int, b: Int, o: Double = 1.0): TinkerColor =
    TinkerColor(r, g, b, o)
}
