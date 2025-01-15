package me.micseydel.testsupport

import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.actor.typed.scaladsl.Behaviors
import akka.testkit.TestLatch
import akka.util.Helpers.Requiring
import akka.util.Timeout
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.NoOp

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class VirtualNoteRef(noteId: String, private var contents: String = "", val helper: Option[ActorRef[NoteRefWatcherHelper.Message]] = None) extends NoteRef(NoteId(noteId)) {

  override def toString: String = s"VirtualNoteRef($noteId)"

  override def readRaw(): Try[String] = {
    //    TestHelpers.log(s"""[[$noteId]].readRaw() returning Success("${TestHelpers.compressedNewlines(contents)}")""")
    Success(contents)
  }

  override def setRaw(contents: String): Try[NoOp.type] = {
    this.contents = contents
    //    TestHelpers.log(s"""[[$noteId]].setRaw("${TestHelpers.compressedNewlines(contents)}") returning Success(NoOp)""")
    helper.foreach { watcher =>
      watcher ! NoteRefWatcherHelper.UpdateMarkdown(contents)
    }
    Success(NoOp)
  }

  override def append(contents: String): Option[Throwable] = {
    this.contents = this.contents + contents
    //    TestHelpers.log(s"""[[$noteId]].append("${TestHelpers.compressedNewlines(contents)}") returning None""")
    None
  }

  override def readRawAsync()(implicit executionContext: ExecutionContext): Future[String] = {
    Future.successful(this.contents)
  }

  // helper

  // FIXME: specify number of writes to ignore?
  def interceptWrite(implicit scheduler: Scheduler): String = {
    import akka.actor.typed.scaladsl.AskPattern._
    implicit val timeout: Timeout = 10.seconds
    val fut = helper.get.ask(NoteRefWatcherHelper.ListenForNextUpdate)
    Await.ready(fut, timeout.duration).value match {
      case Some(Success(value)) => value
      case Some(Failure(exception)) => throw exception
      case None => throw new RuntimeException("Did not expect nothing from future")
    }
  }
}

object NoteRefWatcherHelper {
  sealed trait Message

  case class ListenForNextUpdate(replyTo: ActorRef[String]) extends Message
  case class UpdateMarkdown(markdown: String) extends Message

  def apply(): Behavior[Message] = waitingForListener(None)

  private def waitingForListener(maybeBufferedMarkdown: Option[String]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case ListenForNextUpdate(replyTo) =>
        maybeBufferedMarkdown match {
          case Some(markdown) =>
            replyTo ! markdown
            waitingForListener(None)
          case None =>
            waitingForUpdate(replyTo)
        }
      case UpdateMarkdown(newerMarkdown) =>
        maybeBufferedMarkdown.foreach(md => context.log.warn(s"Dropping old markdown for new: $md"))
        waitingForListener(Some(newerMarkdown))
    }
  }

  private def waitingForUpdate(replyTo: ActorRef[String]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case ListenForNextUpdate(newReplyTo) =>
        if (newReplyTo.path == replyTo.path) {
          Behaviors.same
        } else {
          throw new RuntimeException(s"${newReplyTo.path} tried to replace ${replyTo.path}")
        }
      case UpdateMarkdown(markdown) =>
        replyTo ! markdown
        waitingForListener(None)
    }
  }
}
