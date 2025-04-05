package me.micseydel.dsl.cast

import akka.actor.{ActorPath, Cancellable}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import cats.data.NonEmptyList
import me.micseydel.Common
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TinkerBrain.{PersistedMessage, SentMessage, TranscriptionBroadcast}
import me.micseydel.dsl.cast.TinkerBrainUtil.{cleanerUri, graphForLast3Days, toURIish}
import me.micseydel.dsl.{Sender, SpiritRef, Tinker, TinkerClock, TinkerClockImpl, TinkerColor, Tinkerer}
import me.micseydel.model.NotedTranscription
import me.micseydel.prototyping.WebSocketMessageActor.{SendClientMessage, SendFrame, SendTranscriptionFrame, TinkerEdge}
import me.micseydel.prototyping.{TinkerNode, WebSocketMessageActor}
import me.micseydel.util.{FileSystemUtil, TimeUtil}
import org.slf4j.Logger
import spray.json._

import java.io.FileNotFoundException
import java.nio.file.Path
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.tailrec
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success, Try}

object TinkerBrain {
  sealed trait Message

  case class WriteNote(tinker: Tinker) extends Message

  case class RegisterTinkerer(actorPath: ActorPath, tinkerer: Tinkerer[_]) extends Message

  sealed trait PersistedMessage extends Message {
    def time: ZonedDateTime

    def sender: String

    def receivers: Set[String]

    def participants: Set[String] = (receivers + sender).map(cleanerUri)

    def edges: List[TinkerEdge] = receivers.map(receiver => TinkerEdge(cleanerUri(sender), cleanerUri(receiver))).toList
  }

  final case class SentMessage(approxSentTime: ZonedDateTime, messageType: String, sender: String, receiver: String) extends PersistedMessage {
    override def time: ZonedDateTime = approxSentTime

    override def receivers: Set[String] = Set(receiver)
  }

  object SentMessage {
    def apply(approxSentTime: ZonedDateTime, messageType: String, sender: Sender, receiver: ActorRef[_]): SentMessage = {
      new SentMessage(
        approxSentTime,
        messageType,
        toURIish(sender.path.toSerializationFormat),
        toURIish(receiver.path.toSerializationFormat)
      )
    }
  }

  final case class TranscriptionBroadcast(approxSentTime: ZonedDateTime, messageType: String, sender: String, receivers: Set[String], notedTranscription: NotedTranscription) extends PersistedMessage {
    override def time: ZonedDateTime = approxSentTime
  }

  object TranscriptionBroadcast {
    def apply(approxSentTime: ZonedDateTime, messageType: String, sender: Sender, spirits: Set[SpiritRef[NotedTranscription]], notedTranscription: NotedTranscription): TranscriptionBroadcast = {
      new TranscriptionBroadcast(
        approxSentTime,
        messageType,
        toURIish(sender.path.toSerializationFormat),
        spirits.map(_.path.toSerializationFormat).map(toURIish),
        notedTranscription
      )
    }
  }

  final case class SpiritCast(actorPath: ActorPath) extends Message

  final case class SystemStarted() extends Message
  //  final case class ApplicationStarted() extends Message

  case class RegisterClient(replyTo: WebSocketMessageActor.Client) extends Message

  // behaviors

  def apply(jsonPath: Path, tinkerers: Map[ActorPath, Tinkerer[_]]): Behavior[Message] = Behaviors.setup { context =>
    context.log.info(s"Starting persistence for message tracking")

    import TinkerBrainJsonProtocol.PersistedMessageJsonFormat

    def appendToJsonl(message: PersistedMessage): Unit = {
      // FIXME: use logging instead!
      val filenameWithoutExtension = s"tinker_cast_messages_${TimeUtil.zonedDateTimeToISO8601Date(message.time)}"
      val fullPath = jsonPath.resolve(filenameWithoutExtension + ".jsonl")
      val jsonLine = message.toJson(TinkerBrainJsonProtocol.PersistedMessageJsonFormat).compactPrint
      FileSystemUtil.appendToPath(fullPath, jsonLine)
    }

    def readJsonl(forDay: LocalDate): List[PersistedMessage] = {
      val filenameWithoutExtension = s"tinker_cast_messages_${TimeUtil.localDateTimeToISO8601Date(forDay)}"
      val fullPath = jsonPath.resolve(filenameWithoutExtension + ".jsonl")
      Try(FileSystemUtil.getPathLines(fullPath).map(_.parseJson.convertTo[PersistedMessage])) match {
        case Success(value) => value
        case Failure(_: FileNotFoundException) => List()
        case Failure(other) => throw other
      }
    }

    val webSocketMessageActor: ActorRef[WebSocketMessageActor.Command] = context.spawn(WebSocketMessageActor(), "WebSocketMessageActor")

    systemStarting(appendToJsonl, readJsonl, webSocketMessageActor, tinkerers)
  }

  // states: starting / started

  private def systemStarting(appendToJsonl: PersistedMessage => Unit, readJson: LocalDate => List[PersistedMessage], webSocketMessageActor: ActorRef[WebSocketMessageActor.Command], tinkerers: Map[ActorPath, Tinkerer[_]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    context.log.debug(s"Received TinkerBrain message $message in systemStarting state")

    message match {
      case sm@SentMessage(_, _, _, _) =>
        appendToJsonl(sm)
        Behaviors.same

      case tb@TranscriptionBroadcast(approxSentTime, messageType, sender, receivers, _) =>
        appendToJsonl(tb)
        Behaviors.same

      case SpiritCast(_) =>
        context.log.debug(s"Ignoring SpiritCast $message")
        Behaviors.same

      case SystemStarted() =>

        // FIXME: experiment - using nodes and edges extending back a couple days, though keeping frames for just today
        // FIXME: clock HACK
        val (nodes: Set[TinkerNode], edges: Set[TinkerEdge], existingFrames: List[List[TinkerEdge]]) =
          graphForLast3Days(readJson, tinkerers)(context.log, new TinkerClockImpl())

        context.log.info(s"Sending a graph with ${nodes.size} nodes and ${edges.size} edges")
        webSocketMessageActor ! WebSocketMessageActor.SendGraph(nodes, edges, existingFrames)
        val batcher = context.spawn(RealtimeFrameBatcher(150.milliseconds, webSocketMessageActor), "Batcher")
        systemStarted(appendToJsonl, readJson, webSocketMessageActor, batcher, tinkerers)

      //      case ApplicationStarted() =>
      //        // applicationS?
      //        // send actors+hierarchy to client
      //        context.log.warn("Ignoring ApplicationStarted")
      //        Behaviors.same

      //      case Transcription(notedTranscription) =>
      //        context.log.warn(s"Ignoring transcription")
      //        Behaviors.same

      case RegisterClient(client) =>
        webSocketMessageActor ! WebSocketMessageActor.RegisterClient(client)
        Behaviors.same

      case RegisterTinkerer(actorPath, tinkerer) =>
        systemStarting(
          appendToJsonl, readJson, webSocketMessageActor,
          tinkerers.updated(actorPath, tinkerer)
        )

      case WriteNote(tinker) =>
        val (nodes: Set[TinkerNode], edges: Set[TinkerEdge], _) =
          graphForLast3Days(readJson, tinkerers)(context.log, new TinkerClockImpl())
        // FIXME: hacky
        context.spawnAnonymous(TinkerNoteWriter(nodes, edges, tinkerers)(tinker))
        Behaviors.same
    }
  }

  private def systemStarted(appendToJsonl: PersistedMessage => Unit, readJson: LocalDate => List[PersistedMessage], webSocketMessageActor: ActorRef[WebSocketMessageActor.Command], batcher: ActorRef[RealtimeFrameBatcher.Message], tinkerers: Map[ActorPath, Tinkerer[_]]): Behavior[Message] = Behaviors.receive { (context, message) =>
    context.log.debug(s"Received TinkerBrain message $message in systemStarted state")

    message match {
      case sm@SentMessage(_, _, sender, receiver) =>
        appendToJsonl(sm)
        //        val singleMemberSet = Set(TinkerEdge(cleanerUri(sender), cleanerUri(receiver)))
        batcher ! RealtimeFrameBatcher.Receive(WebSocketMessageActor.SendFrame(sm.edges.distinct))

      case tb@TranscriptionBroadcast(approxSentTime, messageType, sender, receivers, notedTranscription) =>
        receivers.foreach { receiver =>
          appendToJsonl(SentMessage(approxSentTime, messageType, sender, receiver))
        }

        batcher ! RealtimeFrameBatcher.Receive(WebSocketMessageActor.SendTranscriptionFrame(
          tb.edges.toSet,
          notedTranscription
        ))

      case SpiritCast(_) =>
        context.log.debug(s"Ignoring $message")
      case SystemStarted() =>
        val (nodes: Set[TinkerNode], edges: Set[TinkerEdge], existingFrames: List[List[TinkerEdge]]) = graphForLast3Days(readJson, tinkerers)(context.log, new TinkerClockImpl)
        context.log.info(s"Sending a graph with ${nodes.size} nodes and ${edges.size} edges, first existing frame ${existingFrames.headOption}")
        webSocketMessageActor ! WebSocketMessageActor.SendGraph(nodes, edges, existingFrames)

      case RegisterClient(client) =>
        context.log.info(s"Registering a new client after system started then sending SystemStarted to self (also resetting the batcher for subsequent live frames)")
        webSocketMessageActor ! WebSocketMessageActor.RegisterClient(client)
        batcher ! RealtimeFrameBatcher.Reset()
        context.self ! SystemStarted()

      case RegisterTinkerer(actorPath, tinkerer) =>
        systemStarted(
          appendToJsonl, readJson, webSocketMessageActor, batcher,
          tinkerers.updated(actorPath, tinkerer)
        )

      case WriteNote(tinker) =>
        val (nodes: Set[TinkerNode], edges: Set[TinkerEdge], _) =
          graphForLast3Days(readJson, tinkerers)(context.log, new TinkerClockImpl())
        // FIXME: hacky
        context.spawnAnonymous(TinkerNoteWriter(nodes, edges, tinkerers)(tinker))
    }

    Behaviors.same
  }
}

private object TinkerNoteWriter {
  sealed trait Message

  def apply(nodes: Set[TinkerNode], edges: Set[TinkerEdge], tinkerers: Map[ActorPath, Tinkerer[_]])(implicit Tinker: Tinker): Ability[Message] = Tinker.initializedWithNote("Tinker Brain") { (context, noteRef) =>
    context.actorContext.log.info(s"Writing note")

    val formattedNodes = nodes.map {
      case TinkerNode(id, color, emoji, x, y, href) =>
        s"""${id.split('/').last}
           |    - `${id.drop(17)}`
           |    - $emoji $color""".stripMargin
    }.mkString("- ", "\n- ", "")

    val formattedEdges = edges.map {
      case TinkerEdge(source, target) =>
        s"`$source` -> `$target`"
    }.mkString("- ", "\n- ", "")

    val formattedTinkerers = tinkerers.map { case (key, tinkerer) =>
      s"- $key\n    - $tinkerer"
    }.mkString("\n")

    noteRef.setMarkdown(
      s"""- Generated ${context.system.clock.now()}
         |- Nodes: ${nodes.size}
         |- Edges: ${edges.size}
         |- Registered tinkerers: ${tinkerers.size}
         |# Nodes
         |
         |$formattedNodes
         |
         |# Edges
         |
         |$formattedEdges
         |
         |# Registered Tinkerers
         |
         |$formattedTinkerers
         |""".stripMargin)

    context.actorContext.log.info(s"Done writing note")

    Tinker.done
  }
}

private object TinkerBrainUtil {
  // utils

  //  private val Prefix = "akka://AkkaActor/system/TinkerCast/"
  //  private val CharsToDrop = Prefix.length

  def cleanerUri(uri: String): String = {
    val adaptersRemoved = uri.split("/").toList.reverse.dropWhile(_.startsWith("$")).reverse.mkString("/")
    adaptersRemoved.drop(7)
    //    if (adaptersRemoved.startsWith(Prefix)) {
    //      adaptersRemoved.drop(CharsToDrop)
    //    } else {
    //      adaptersRemoved
    //    }

    //    val soFar = uri.substring(7)
    //    if (soFar.endsWith("/$$a-adapter")) {
    //      soFar.dropRight(12)
    //    } else {
    //      soFar
    //    }
  }

  import java.time.ZonedDateTime

  // Function to round a ZonedDateTime to the nearest minute
  //  def roundToMinute(time: ZonedDateTime): ZonedDateTime = {
  //    time.withSecond(0).withNano(0)
  //  }

  // Function to bucket messages by minute
  def bucketMessagesByMinute(messages: List[PersistedMessage]): List[List[TinkerEdge]] = {
    val buckets: Map[ZonedDateTime, List[TinkerEdge]] = messages.groupBy(_.time.truncatedTo(ChronoUnit.MINUTES))
      .map { case (minute, bucket) =>
        minute -> bucket.flatMap(_.edges)
      }

    buckets.toList.sortBy(_._1).map(_._2)

    //      .values
    //      .toList
    //      .map(_.flatMap(_.edges))

    //    messages.groupBy(_.time.truncatedTo(ChronoUnit.MINUTES))
    //      .values
    //      .toList
    //      .map(_.flatMap(_.edges))
  }

  def graphForLast3Days(readJson: LocalDate => List[PersistedMessage], tinkerers: Map[ActorPath, Tinkerer[_]])(implicit log: Logger, clock: TinkerClock): (Set[TinkerNode], Set[TinkerEdge], List[List[TinkerEdge]]) = {
    val today = readJson(LocalDate.now())
    val yesterday = readJson(LocalDate.now().minusDays(1))
    val theDayBeforeYesterday = readJson(LocalDate.now().minusDays(2))

    val combined = today ::: yesterday ::: theDayBeforeYesterday

    log.debug(s"Gathered messages: ${today.size} today, ${yesterday.size} yesterday and ${theDayBeforeYesterday.size} the day before yesterday; total ${combined.size}")

    val tinkerersRemapped: Map[String, Tinkerer[_]] = tinkerers.toList.map { case (path, value) => toURIish(path.toSerializationFormat.drop(7)) -> value }.toMap

    val (nodes: Set[TinkerNode], edges: Set[TinkerEdge], _tooManyExistingFrames: List[List[TinkerEdge]]) = graphFor(combined, tinkerersRemapped)

    val (_tooFewNodes: Set[TinkerNode], _tooFewEdges: Set[TinkerEdge], todaysFrames: List[List[TinkerEdge]]) = graphFor(today, tinkerersRemapped)

    log.debug(s"Using counts for nodes, edges and frames: ${nodes.size}, ${edges.size}, ${todaysFrames.size} and not using ${_tooFewNodes.size}, ${_tooFewEdges.size}, ${_tooManyExistingFrames.size}")

    // FIXME: hacky but the graph looks awful without this
    (
      nodes
        .filterNot(_.id.contains("TranscriptionNoteWrapper_"))
        .filterNot(_.id.contains("ChroniclerMOC"))
        .filterNot(_.id.contains("Operator"))
        // FIXME can be removed after a couple days
        .filterNot(_.id.contains("HypothesisListener"))
      ,
      edges.filterNot {
        case TinkerEdge(source, target) =>
          source.contains("TranscriptionNoteWrapper_") || target.contains("TranscriptionNoteWrapper_") ||
            source.contains("ChroniclerMOC") || target.contains("ChroniclerMOC") ||
            source.contains("Operator") || target.contains("Operator") || source.contains("HypothesisListener") || target.contains("HypothesisListener")
      },
      todaysFrames
    )
    //    (nodes, edges, todaysFrames)
  }

  def graphFor(messages: List[PersistedMessage], tinkerers: Map[String, Tinkerer[_]])(implicit log: Logger, clock: TinkerClock): (Set[TinkerNode], Set[TinkerEdge], List[List[TinkerEdge]]) = {
    val uniqueNodeNames: Set[String] = messages.flatMap(_.participants).toSet
    log.debug(s"uniqueNodeNames ${uniqueNodeNames}")

    val edges = messages.flatMap(_.edges).toSet

    val nodesAndMaybeRemappings = uniqueNodeNames
      .map { nodeName =>
        val nodeNameParts = nodeName.split("/")
        (0 until nodeNameParts.size).map { dropNum =>
          nodeNameParts
            .dropRight(dropNum)
            .mkString("/")
        }.map(nodeOrSuperVisor => tinkerers.get(nodeOrSuperVisor).map(nodeOrSuperVisor -> _)).collectFirst {
          case Some(tinkerer) => tinkerer
        } match {
          case Some(nodeOrSuperVisor -> Tinkerer(color, emoji, href)) =>
            val maybeRemap = if (nodeOrSuperVisor != nodeName) {
              Some(nodeName -> nodeOrSuperVisor)
            } else {
              None
            }
            (TinkerNode(
              nodeOrSuperVisor,
              color.toString,
              Some(emoji),
              None, None,
              href
            ), maybeRemap)

          case None =>
            log.debug(s"nodeName $nodeName not found in ${tinkerers.keys}")

            if (nodeName.endsWith("Operator")) {
              (TinkerNode(
                nodeName,
                TinkerColor(0, 0, 0).toString,
                Some("☎️"), None, None, None
              ), None)
            } else if (nodeName.endsWith("VaultKeeper")) {
              (TinkerNode(
                nodeName,
                TinkerColor(151, 7, 223).toString,
                Some("🏦"), None, None, None
              ), None)
            } else {
              (TinkerNode(
                nodeName,
                TinkerColor.random().toString,
                None, None, None, None
              ), None)
            }
        }
      }

    val nodes = nodesAndMaybeRemappings.map(_._1)

    val remappings = nodesAndMaybeRemappings.flatMap(_._2).toMap

    val priorFrames = bucketMessagesByMinute(messages)

    (nodes, edges.map {
      case TinkerEdge(source, target) =>
        TinkerEdge(remappings.getOrElse(source, source), remappings.getOrElse(target, target))
    }, priorFrames)
  }

  implicit class Listeners(spirits: Set[SpiritRef[NotedTranscription]]) {
    def *!*(nt: NotedTranscription)(implicit sender: Sender, tinkerBrain: ActorRef[TinkerBrain.Message]): Unit = {
      tinkerBrain ! TinkerBrain.TranscriptionBroadcast(
        ZonedDateTime.now(),
        classOf[NotedTranscription].getName,
        sender,
        spirits,
        nt
      )
      spirits.foreach(_ !!!! nt)
    }
  }

  def toURIish(serializationFormat: String): String = {
    serializationFormat.split("#").toList match {
      case List(alreadyClean) => alreadyClean
      case List(desired, _) => desired
      case _ => serializationFormat
    }
  }
}

private object RealtimeFrameBatcher {
  sealed trait Message

  final case class Receive(toForward: SendClientMessage) extends Message

  final case class Reset() extends Message

  private case class Burst() extends Message

  def apply(batchWindow: FiniteDuration, webSocketMessageActor: ActorRef[WebSocketMessageActor.Command]): Behavior[Message] = Behaviors.setup { context =>
    context.log.info("Initializing...")
    idle(batchWindow, webSocketMessageActor)
  }

  // behaviors

  private def idle(batchWindow: FiniteDuration, webSocketMessageActor: ActorRef[WebSocketMessageActor.Command]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case Receive(toForward) =>
        toForward match {
          case sf@SendFrame(_) =>
            context.log.info(s"Received a SendFrame while idle, batching with batch window $batchWindow and scheduling a burst")
            val cancellable: Cancellable = context.scheduleOnce(batchWindow, context.self, Burst())
            batching(batchWindow, webSocketMessageActor)(cancellable, NonEmptyList.of(sf))
          case stf@WebSocketMessageActor.SendTranscriptionFrame(_, _) =>
            context.log.info(s"Received a SendTranscriptionFrame while idle, sending immediately")
            webSocketMessageActor ! stf
            Behaviors.same
          case WebSocketMessageActor.SendHeartbeat() | WebSocketMessageActor.SendGraph(_, _, _) =>
            context.log.warn(s"Received $toForward but should have only been sent types SendFrame and SendTranscriptionFrame, ignoring")
            Behaviors.same
        }

      case unexpected@Burst() =>
        context.log.warn(s"Received $unexpected while idle")
        Behaviors.same

      case unexpected@(Reset()) =>
        context.log.debug(s"Received $unexpected while idle") // FIXME: this happens a lot, why?
        Behaviors.same
    }
  }

  private def batching(batchWindow: FiniteDuration, webSocketMessageActor: ActorRef[WebSocketMessageActor.Command])(cancellable: Cancellable, buffer: NonEmptyList[SendClientMessage]): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case Receive(sendClientMessage: SendClientMessage) =>
        context.log.debug("Batching another SendClientMessage")
        batching(batchWindow, webSocketMessageActor)(cancellable, sendClientMessage :: buffer)
      case Burst() =>
        val (batched, maybeRemainder) = batchWithRemainder(buffer)
        context.log.debug("Sending a batch before processing remainder")
        webSocketMessageActor ! batched
        maybeRemainder match {
          case Some(remainder) =>
            context.log.debug("Burst complete, scheduling another...")
            val replacementCancellable: Cancellable = context.scheduleOnce(batchWindow, context.self, Burst())
            batching(batchWindow, webSocketMessageActor)(replacementCancellable, remainder)
          case None =>
            context.log.info("Switching to idle...")
            idle(batchWindow, webSocketMessageActor)
        }
      case Reset() =>
        cancellable.cancel()
        idle(batchWindow, webSocketMessageActor)
    }
  }

  // util

  private def batchWithRemainder(buffer: NonEmptyList[SendClientMessage]): (SendClientMessage, Option[NonEmptyList[SendClientMessage]]) = {
    @tailrec
    def helper(listBuffer: List[SendClientMessage], accumulator: Set[TinkerEdge]): (SendClientMessage, Option[NonEmptyList[SendClientMessage]]) = {
      listBuffer match {
        case head :: theRest =>
          head match {
            case SendFrame(newEdges) =>
              helper(theRest, accumulator.union(newEdges.toSet))

            case SendTranscriptionFrame(_, _) =>
              (SendFrame(accumulator.toList), NonEmptyList.fromList(listBuffer))

            case unexpected@(WebSocketMessageActor.SendHeartbeat() | WebSocketMessageActor.SendGraph(_, _, _)) =>
              throw new RuntimeException(s"Really did not expect $unexpected right now, should have already filtered it out")
          }

        case Nil =>
          (SendFrame(accumulator.toList), None)
      }
    }

    (buffer.head, buffer.tail) match {
      case (SendFrame(edges), tail) =>
        helper(tail, edges.toSet)

      case (stf@SendTranscriptionFrame(_, _), tail) =>
        val maybeRemainder: Option[NonEmptyList[SendClientMessage]] = tail match {
          case Nil =>
            None
          case head :: theRest =>
            Some(NonEmptyList.of(head, theRest.toIndexedSeq: _*))
        }
        (stf, maybeRemainder)

      case (unexpected@(WebSocketMessageActor.SendHeartbeat() | WebSocketMessageActor.SendGraph(_, _, _)), _) =>
        throw new RuntimeException(s"Really did not expect $unexpected right now, should have already filtered it out")
    }
  }
}



// Non-actor stuff

object TinkerBrainJsonProtocol extends DefaultJsonProtocol {

  import me.micseydel.Common.ZonedDateTimeJsonFormat

  implicit val sentMessageFormat: JsonFormat[SentMessage] = jsonFormat4(SentMessage.apply(_: ZonedDateTime, _: String, _: String, _: String))
  import NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat

  implicit val transcriptionBroadcastFormat: JsonFormat[TranscriptionBroadcast] =
    jsonFormat5(TranscriptionBroadcast.apply(
      _: ZonedDateTime,
      _: String,
      _: String,
      _: Set[String],
      _: NotedTranscription
    ))

  implicit object PersistedMessageJsonFormat extends RootJsonFormat[PersistedMessage] {
    def write(m: PersistedMessage): JsValue = {
      val (jsObj, typ) = m match {
        case s: SentMessage => (s.toJson.asJsObject, "SentMessage")
        case s: TranscriptionBroadcast => (s.toJson.asJsObject, "TranscriptionBroadcast")
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): PersistedMessage = {
      value.asJsObject.getFields("type") match {
        case Seq(JsString("TranscriptionBroadcast")) => value.convertTo[TranscriptionBroadcast]
        case Seq(JsString("SentMessage")) => value.convertTo[SentMessage]
        // FIXME SentMessage fallback, remove after it runs
        case Seq() => value.convertTo[SentMessage]
        case other => throw DeserializationException(s"Unknown type, expected ReceivedMessage or SentMessage (in a Seq) but got $other")
      }
    }
  }

  implicit val messageListJsonFormat: RootJsonFormat[List[SentMessage]] = listFormat(sentMessageFormat)
}
