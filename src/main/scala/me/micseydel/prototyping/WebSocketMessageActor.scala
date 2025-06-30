package me.micseydel.prototyping

import akka.actor.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model.ws.TextMessage
import me.micseydel.model.NotedTranscription
import me.micseydel.prototyping.WebSocketMessageActor._
import me.micseydel.util.StringImplicits.RichString
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt

object WebSocketMessageActor {
  type Client = akka.actor.ActorRef

  // mailbox

  sealed trait Command
  case class RegisterClient(replyTo: Client) extends Command
  private case class Heartbeat() extends Command

  // for serialization to send to the client
  sealed abstract class SendClientMessage(val messageType: String) extends Command

  case class TinkerEdge(source: String, target: String)
  case class SendGraph(nodes: Set[TinkerNode], edges: Set[TinkerEdge], priorFrames: List[List[TinkerEdge]]) extends SendClientMessage("graph") {
    def addFrames(frames: List[List[TinkerEdge]]): SendGraph = {
      this.copy(priorFrames = frames ::: priorFrames)
    }
  }
  case class SendFrame(edges: List[TinkerEdge]) extends SendClientMessage("frame")
  case class SendInputFrame(edges: Set[TinkerEdge], kind: String) extends SendClientMessage("input_frame")
  case class SendOutputFrame(edges: Set[TinkerEdge], kind: String) extends SendClientMessage("output_frame")
  case class SendHeartbeat() extends SendClientMessage("heartbeat")

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    context.log.info("initializing...")
    initializing(Initializing.empty)
  }

  //

  private case class Initializing(initReplyTo: Option[Client], initGraph: Option[SendGraph], bufferedFrames: List[List[TinkerEdge]]) {
    def initialized(): Option[(Client, SendGraph)] = for {
      replyTo <- initReplyTo
      graph <- initGraph
    } yield (replyTo, graph.addFrames(bufferedFrames.reverse))

    def addFrame(frame: List[TinkerEdge]): Initializing = this.copy(bufferedFrames = frame :: bufferedFrames)
  }

  private case object Initializing {
    def empty: Initializing = Initializing(None, None, Nil)
  }

  private def initializing(init: Initializing): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case RegisterClient(replyTo) =>
        context.log.info(s"Registering client ${replyTo.path.toSerializationFormat} and starting heartbeat")
        val updated = init.copy(initReplyTo = Some(replyTo))
        updated.initialized() match {
          case Some((replyTo, graph)) =>
            start(replyTo, graph)

          case None =>
            context.log.info("Received replyTo but still initializing (waiting for graph)...")
            initializing(updated)
        }

      case graph@SendGraph(nodes, edges, frames) =>
        context.log.debug(s"Received graph with ${nodes.size} nodes, ${edges.size} edges and ${frames.size} frames")
        val updated = init.copy(initGraph = Some(graph))
        updated.initialized() match {
          case Some((replyTo, graph)) =>
            start(replyTo, graph)

          case None =>
            context.log.info("Received graph but still initializing (waiting for replyTo)...")
            initializing(updated)
        }

      case SendFrame(edges) =>
        val updated = init.addFrame(edges)
        context.log.debug(s"Received SendFrame, buffering frame with ${edges.size} edges, ${updated.bufferedFrames.size} frames now buffered")
        initializing(updated)

      case m@(_: SendClientMessage | _: Heartbeat) =>
        context.log.debug(s"SendClientMessage command ${m.getClass} received but no client to send it to yet")
        Behaviors.same
    }
  }

  private def start(replyTo: ActorRef, graph: SendGraph): Behavior[Command] = Behaviors.setup { context =>
    context.log.info("Initialized, starting Heartbeat...")
    context.self ! Heartbeat()
    behavior(replyTo, graph)
  }

  private def behavior(replyTo: ActorRef, graph: SendGraph): Behavior[Command] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case RegisterClient(newReplyTo) =>
        context.log.info(s"Replaced replyTo ${replyTo.path.toSerializationFormat} with ${newReplyTo.path.toSerializationFormat}") //, using graph $graph")
        behavior(newReplyTo, graph)

      case sendClientMessage: SendClientMessage =>
        context.log.debug(s"Received SendClientMessage $sendClientMessage...")
        // FIXME: what is the fuckery around needing to pass this explicit to one of the below and not the other, but both fail compilation when I don't fix just the first one?
        import WebSocketMessageJsonProtocol.SendClientMessageJsonFormat
        val forClient = TextMessage(sendClientMessage.toJson(SendClientMessageJsonFormat).compactPrint)
        sendClientMessage match {
          case sg@SendGraph(nodes, edges, priorFrames) =>
            context.log.info(s"Sending a graph with ${nodes.size} nodes and ${edges.size} edges, ${priorFrames.size} initial frames")

            @tailrec
            def cleanup(remaining: List[List[TinkerEdge]], soFar: List[List[TinkerEdge]] = Nil): List[List[TinkerEdge]] = {
              remaining match {
                case Nil => soFar
                case head :: tail =>
                  if (soFar.headOption.map(_.distinct.toSet).contains(head.distinct.toSet)) {
                    // if this element is the same as the last, just drop it
                    cleanup(tail, soFar)
                  } else {
                    cleanup(tail, head :: soFar)
                  }

              }
            }

            replyTo ! TextMessage((sg.copy(priorFrames = cleanup(priorFrames).reverse): SendClientMessage).toJson.compactPrint)
            Behaviors.same


          case sf@(SendInputFrame(_, _) | SendOutputFrame(_, _)) =>
            context.log.info(s"Sending IO frame ${sf.getClass}")
            replyTo ! forClient
            behavior(replyTo, graph)
            Behaviors.same

          case SendFrame(edges) =>
            context.log.info(s"Sending a frame with ${edges.size} edges")
            replyTo ! forClient
            behavior(replyTo, graph.addFrames(List(edges)))
            Behaviors.same
          case SendHeartbeat() =>
            context.log.debug(s"Sending heartbeat")
            replyTo ! forClient
            Behaviors.same
        }

        case Heartbeat() =>
          context.log.debug("Heartbeat")
          context.self ! SendHeartbeat()
          context.scheduleOnce(10.seconds, context.self, Heartbeat())
          Behaviors.same
    }
  }
}

/*
export interface TinkerNode extends d3.SimulationNodeDatum {
    id: string;
    color: string;
    emoji?: string;
    x?: number;
    y?: number;
}
 */

case class TinkerNode(id: String, color: String, emoji: Option[String], x: Option[Int], y: Option[Int], href: Option[String])

object WebSocketMessageJsonProtocol extends DefaultJsonProtocol {

  implicit val tinkerNodeFormat: JsonFormat[TinkerNode] = jsonFormat6(TinkerNode)
  import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat

  implicit val tinkerEdgeFormat: JsonFormat[TinkerEdge] = jsonFormat2(TinkerEdge)
  implicit val sendGraphFormat: JsonFormat[SendGraph] = jsonFormat3(SendGraph)
  implicit val sendInputFrameFormat: JsonFormat[SendInputFrame] = jsonFormat2(SendInputFrame)
  implicit val sendOutputFrameFormat: JsonFormat[SendOutputFrame] = jsonFormat2(SendOutputFrame)
  implicit val sendFrameFormat: JsonFormat[SendFrame] = jsonFormat1(SendFrame)
  implicit val sendHeartbeatFormat: JsonFormat[SendHeartbeat] = jsonFormat0(SendHeartbeat)

  implicit object SendClientMessageJsonFormat extends JsonFormat[SendClientMessage] {
    def write(m: SendClientMessage): JsValue = {
      val (jsObj, typ) = m match {
        case s: SendGraph => (s.toJson.asJsObject, s.messageType)
        case s: SendInputFrame => (s.toJson.asJsObject, s.messageType)
        case s: SendOutputFrame => (s.toJson.asJsObject, s.messageType)
        case s: SendFrame => (s.toJson.asJsObject, s.messageType)
        case s: SendHeartbeat => (s.toJson.asJsObject, s.messageType)
      }
      JsObject(jsObj.fields + ("type" -> JsString(typ)))
    }

    def read(value: JsValue): SendClientMessage = {
      value.asJsObject.getFields("type") match {
//        case Seq(JsString("SendGraph")) => value.convertTo[SendGraph]
//        case Seq(JsString("SendTranscription")) => value.convertTo[SendTranscription]
//        case Seq(JsString("SendFrame")) => value.convertTo[SendFrame]
        case other => throw DeserializationException(s"Unknown type, expected ReceivedMessage or SentMessage (in a Seq) but got $other")
      }
    }
  }

  implicit val messageListJsonFormat: RootJsonFormat[List[SendClientMessage]] = listFormat(SendClientMessageJsonFormat)
}
