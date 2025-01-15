package me.micseydel.actor.inactive

import java.nio.file.Path

object MobileTextCaptureActor {
  sealed trait Message
  private case class PathModified(path: Path) extends Message
  private case class FileRead(lines: List[String]) extends Message
  private case class UnexpectedState(reason: String, throwable: Option[Throwable] = None) extends Message

//  def apply(watchPath: Path): Behavior[Message] = Behaviors.setup[Message] { context =>
//    val adapter: ActorRef[PathUpdatedEvent] = context.messageAdapter {
//      case PathModifiedEvent(path) => PathModified(path)
//      case PathCreatedEvent(path) => PathModified(path)
//      case e@PathDeletedEvent(_) =>
//        UnexpectedState(e.toString)
//    }
//    @unused // spawns its own thread, so we subscribe to it via an adapter
//    val folderWatcherActor = context.spawn(FolderWatcherActor(watchPath, adapter), "FolderWatcher")
//
//    readFile(watchPath.resolve(expectedFilename), context)
//    initializing()
//  }
//
//  private def initializing(): Behavior[Message] = Behaviors.receive { (context, message) =>
//    message match {
//      case FileRead(lines) =>
//        val captures = linesToCaptures(lines, context.log)
//        val latestReference = captures.lastOption.map(_.reference).getOrElse("(nothing)")
//        context.log.info(s"Initialized with ${lines.size} with most recent reference $latestReference")
//        behavior(captures.map(_.reference).toSet)
//      case PathModified(path) =>
//        context.log.warn(s"Didn't expect PathModified($path) while initializing")
//        Behaviors.same
//      case UnexpectedState(reason, maybeThrowable) =>
//        maybeThrowable match {
//          case Some(throwable) =>
//            context.log.error(s"Unexpected state while initializing: $reason", throwable)
//          case None =>
//            context.log.error(s"Unexpected state while initializing: $reason")
//        }
//        Behaviors.same
//    }
//  }
//
//  private def behavior(seenReferences: Set[String]): Behavior[Message] = Behaviors.receive { (context, message) =>
//    message match {
//      case PathModified(path) =>
//        if (path.getFileName.toString != expectedFilename) {
//          context.log.warn(s"Unexpected path update $path (expected filename $expectedFilename)")
//        } else {
//          readFile(path, context)
//        }
//        Behaviors.same
//      case FileRead(lines) =>
//        val captures: List[TextCaptureLine] = linesToCaptures(lines, context.log)
//
//        for (capture <- captures) {
//          if (seenReferences.contains(capture.reference)) {
//            context.log.debug(s"Already seen ${capture.reference}")
//          } else {
//            context.log.info(s"New capture: $capture")
//          }
//        }
//        behavior(captures.map(_.reference).toSet)
//      case UnexpectedState(message, maybeThrowable) =>
//        maybeThrowable match {
//          case Some(throwable) =>
//            context.log.error(message, throwable)
//          case None =>
//            context.log.error(message)
//        }
//        Behaviors.same
//    }
//  }
//
//  private def readFile(path: Path, context: ActorContext[Message]): Unit = {
//    implicit val blockingExecutionContext: ExecutionContext =
//      context.system.dispatchers.lookup(DispatcherSelector.blocking())
//    val fut: Future[List[String]] = Future {
//      if (Files.exists(path))  {
//        Files.readAllLines(path).asScala.toList
//      } else {
//        List()
//      }
//    }
//    context.pipeToSelf(fut) {
//      case Success(value) =>
//        FileRead(value)
//      case Failure(exception) =>
//        UnexpectedState("Unexpected exception while reading file", Some(exception))
//    }
//  }
//
//  // private
//
//  private case class TextCaptureLine(raw: String, completed: Boolean, time: ZonedDateTime, contents: String, reference: String)
//
//  private def expectedFilename: String = {
//    val date = LocalDate.now()
//    val formatter = DateTimeFormatter.ofPattern("M-d-yy")
//    val dateString = date.format(formatter)
//    f"Mobile Text Capture ($dateString).md"
//  }
//
//  private def linesToCaptures(lines: List[String], log: Logger): List[TextCaptureLine] = {
//    lines.map(lineToTextCaptureLine).flatMap {
//      case Right(capture) =>
//        Some(capture)
//      case Left(line) =>
//        log.warn(s"Failed to parse line:- $line")
//        None
//    }
//  }
//
//  private def lineToTextCaptureLine(line: String): Either[String, TextCaptureLine] = {
//    // e.g. "- [ ] \\[17.53\\] testing freaking regular expressions ^mobile-cap-1689814391"
//    val TextPattern = (
//      """- """ + // Matches the initial dash and space.
//        """\[(.)\] """ + // Matches the completion status (x or empty) inside brackets and a space.
//        """\\\[(\d\d\.\d\d)\\] """ + // Matches the time inside brackets and a space.
//        """(.*?) """ + // Matches the contents until it hits the reference.
//        """\^(.*)""" // Matches the reference, starting with '^' till the end of the line.
//      ).r
//
//
//    val formatter = DateTimeFormatter.ofPattern("HH.mm")
//    line match {
//      case TextPattern(completed, time, contents, reference) =>
//        val completedStatus: Boolean = completed.toLowerCase == "x"
//        val timePart = LocalTime.parse(time, formatter)
//        // FIXME: use a clock
//        val datePart: LocalDate = LocalDate.now()
//        val zonedTime = ZonedDateTime.of(datePart, timePart, ZoneId.systemDefault())
//        Right(TextCaptureLine(line, completedStatus, zonedTime, contents, reference))
//      case _ =>
//        Left(line)
//    }
//  }
}
