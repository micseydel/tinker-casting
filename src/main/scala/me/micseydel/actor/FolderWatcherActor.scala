package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import me.micseydel.Common
import me.micseydel.actor.FolderWatcherActor.{NonPathUpdate, PathCreatedEvent, PathDeletedEvent, PathModifiedEvent}

import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW}
import java.nio.file._
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

object FolderWatcherActor {
  sealed trait Message

  sealed trait PathUpdatedEvent extends Message

  // path updates
  case class PathCreatedEvent(path: Path) extends PathUpdatedEvent

  case class PathModifiedEvent(path: Path) extends PathUpdatedEvent

  case class PathDeletedEvent(path: Path) extends PathUpdatedEvent

  // non-path updates
  case class NonPathUpdate(string: String, exception: Option[Exception] = None) extends Message

  // behavior

  def apply(watchPath: Path, subscribers: ActorRef[PathUpdatedEvent]*): Behavior[Message] = Behaviors.setup[Message] { context =>
    // FIXME: actors should not be doing random IO
    Files.createDirectories(watchPath)
    // FIXME: close this down on detecting shutdown
    val folderWatcher = new VaultFolderWatcherThread(watchPath, context.self)
    folderWatcher.start()

    Behaviors.receiveMessage {
      case u@PathCreatedEvent(path) =>
        context.log.debug(s"Sending $path to subscribers $subscribers")
        subscribers.foreach(_ ! u)
        Behaviors.same
      case m@PathModifiedEvent(path) =>
        if (path.getFileName.toString == ".DS_Store") {
          context.log.debug(s"$path MODIFIED, ignoring")
        } else {
          // FIXME: warn when this happens?
          context.log.info(s"Path $path MODIFIED, notifying $subscribers")

          // FIXME for a Whisper Flask listener, this basically shouldn't happen but if it does it will surface reasonably
          // by duplicating in notes in non-destructive ways; in fact, it might help me detect if the audio capture system
          // on Android is itself acting in a destructive manner (solution might be to not take notes too fast)
          subscribers.foreach(_ ! m)
        }

        Behaviors.same

      case NonPathUpdate(message, None) =>
        context.log.error(message)
        Behaviors.same
      case NonPathUpdate(message, Some(exception)) =>
        context.log.error(message, exception)
        Behaviors.same
      case other =>
        context.log.info(s"Not currently handling $other")
        Behaviors.same
    }
  }
}

private class VaultFolderWatcherThread(watchedPath: Path, folderWatcherActor: ActorRef[FolderWatcherActor.Message]) extends Thread {
  private val watchService = FileSystems.getDefault.newWatchService()

  private def handleEvent(event: WatchEvent[_]): Unit = event.kind() match {
    case OVERFLOW =>
      val msg = "OVERFLOW (Per Javadoc for StandardWatchEventKinds: " +
        "A special event to indicate that events may have been lost or discarded.)"
      folderWatcherActor ! NonPathUpdate(msg)
    case ENTRY_CREATE =>
      val child: Path = watchedPath.resolve(event.context().asInstanceOf[Path])
      folderWatcherActor ! PathCreatedEvent(child)
    case ENTRY_DELETE =>
      val child: Path = watchedPath.resolve(event.context().asInstanceOf[Path])
      folderWatcherActor ! PathDeletedEvent(child)
    case ENTRY_MODIFY =>
      val child: Path = watchedPath.resolve(event.context().asInstanceOf[Path])
      folderWatcherActor ! PathModifiedEvent(child)
  }

  override def run(): Unit = {
    try {
      watchedPath.register(
        watchService,
        OVERFLOW, ENTRY_CREATE,
        // FIXME: fastest way to stop deletes from breaking the service because I try to access the file on disk
//        ENTRY_DELETE,
        ENTRY_MODIFY
      )

      while (!Thread.currentThread().isInterrupted) {
        val key: WatchKey = watchService.take()

        val events: mutable.Seq[WatchEvent[Path]] = key.pollEvents().asScala
          .map(event => event.asInstanceOf[WatchEvent[Path]])
        events.sortWith { (e1, e2) =>
          // FIXME: realistically, I have to gather all the data from disk then filter out failures
          Try(eventToCaptureTime(e1) < eventToCaptureTime(e2)) match {
            case Failure(exception: NoSuchFileException) =>
              false // FIXME: HACK
            case Success(value) =>
              value
            case Failure(throwable) =>
              throw throwable
          }
        }

        events.foreach { event =>
          handleEvent(event)
        }

        key.reset()
      }
    } catch {
      case _: InterruptedException =>
        folderWatcherActor ! NonPathUpdate("InterruptedException")
      case e: Exception =>
        val msg = s"Unexpected exception $e"
        folderWatcherActor ! NonPathUpdate(msg, Some(e))
        println(msg)
        e.printStackTrace()
    } finally {
      val msg = "Closing watch service"
      folderWatcherActor ! NonPathUpdate(msg)
      println(msg)
      watchService.close()
    }
  }

  private def eventToCaptureTime(event: WatchEvent[Path]): Long = {
    Common.getFileCreationTime(watchedPath.resolve(event.context())).toEpochSecond
  }

  override def interrupt(): Unit = {
    super.interrupt()
    watchService.close()
  }
}

object FolderWatcherActorTest {
//  private object EchoActor {
//    sealed trait Message
//    private case class Envelope(event: FolderWatcherActor.PathUpdatedEvent) extends Message
//
//    def apply(watchPath: Path)(implicit Tinker: Tinker): Behavior[Message] = Behaviors.setup { context =>
//      val adapter = context.messageAdapter(Envelope)
//      context.spawn(FolderWatcherActor(watchPath, adapter), "FolderWatcherActor")
//
//      def log(string: String): Unit = {
//        println(s"[${Common.zonedDateTimeToISO8601(ZonedDateTime.now())}] $string")
//      }
//
//      Tinker.withMessages {
//        case Envelope(PathCreatedEvent(path)) =>
//          val msg = getCaptureTimeFromFilename(path.getFileName.toString) match {
//            case Right(capturetime_from_filename) =>
//              s"New path $path at $capturetime_from_filename per filename, ${Common.getFileCreationTimeISO8601(path)} by file system"
//            case Left(msg) =>
//              s"New path $path at ${Common.getFileCreationTimeISO8601(path)} by file system, failed to extract from filename: $msg"
//          }
//          context.log.info(msg)
//          log(msg)
//          Tinker.steadily
//        case Envelope(PathModifiedEvent(path)) =>
//          val msg = s"Existing path modified: $path with creation time ${Common.getFileLastModifiedTime(path)} and modified ${Common.getFileLastModifiedTime(path)}"
//          context.log.info(msg)
//          log(msg)
//          Tinker.steadily
//        case other =>
//          val msg = s"Was not expecting $other"
//          context.log.error(msg)
//          Tinker.steadily
//      }
//    }
//  }

//  def f(): Unit = {
//    val filename = "mobile_audio_capture_20231123-131145.wav"
//
//    // Step 1: Define a regex pattern for extracting the date and time
//    val pattern = """(\d{8}-\d{6})""".r
//
//    // Step 2: Match and Extract the date and time
//    pattern.findAllIn(filename) match {
//      case Some(pattern(datetime)) =>
//        val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")
//        val localDateTime = LocalDateTime.parse(datetime, formatter)
//
//        // FIXME: assumes that my system is running in the same place as my phone; true for the foreseeable future
//        val zonedDateTime = ZonedDateTime.of(localDateTime, ZoneId.systemDefault())
//
//        // Output
//        println(zonedDateTime)
//
//      case None =>
//        println("Filename does not match the expected format.")
//    }
//  }
//
//  def g(): Unit = {
//    val filename = "mobile_audio_capture_20231123-131145.wav"
//
//    // Define a regex pattern
//    val pattern = """(\d{8}-\d{6})""".r
//
//    // Define a DateTimeFormatter
//    val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")
//
//    // Function to parse date and time
//    def parseDateTime(dateTimeStr: String): Either[String, ZonedDateTime] = {
//      try {
//        val localDateTime = LocalDateTime.parse(dateTimeStr, formatter)
//        Right(ZonedDateTime.of(localDateTime, ZoneId.systemDefault()))
//      } catch {
//        case e: DateTimeParseException => Left(s"Invalid date format: $dateTimeStr")
//      }
//    }
//
//    // Process each match
//    val results = pattern.findAllIn(filename).map(parseDateTime).toList
//
//    // Example of handling the results
//    results.foreach {
//      case Right(zonedDateTime) => println(s"Success: $zonedDateTime")
//      case Left(errorMsg) => println(s"Error: $errorMsg")
//    }
//  }





//  def main(args: Array[String]): Unit = {
//    implicit val system: ActorSystem[EchoActor.Message] = {
//      val watchPath = stringToPath("/Users/micseydel/obsidian_vaults/withsubms2/deliberate_knowledge_accretion/deliberate_knowledge_accretion_attachments/mobile_audio_captures/")  // FIXME
//      ActorSystem[EchoActor.Message](EchoActor(watchPath), "FolderWatcherActor")
//    }
//  }
//
//  private def stringToPath(pathStr: String): Path = {
//    try {
//      val path = Paths.get(pathStr)
//      path // FIXME: this shouldn't be in the try
//    } catch {
//      case ex: InvalidPathException =>
//        throw ex
//    }
//  }
}
