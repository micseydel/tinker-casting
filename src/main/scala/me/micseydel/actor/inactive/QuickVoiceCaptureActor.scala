package me.micseydel.actor.inactive

import akka.actor.ActorRef as UntypedActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.model.ws.{BinaryMessage, TextMessage, Message as AkkaMessage}
import akka.util.ByteString
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{Tinker, TinkerContext}

import java.io.{ByteArrayInputStream, File}
//import java.nio.file.Path
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import javax.sound.sampled.{AudioFileFormat, AudioFormat, AudioInputStream, AudioSystem}
import scala.concurrent.duration.DurationInt
import scala.reflect.io.Path

object QuickVoiceCaptureActor {
  sealed trait Message

  sealed trait Command extends Message

  case class RegisterClient(client: UntypedActorRef) extends Command

  case class IncomingMessage(text: AkkaMessage) extends Command

  //  case object ClientDisconnected extends Command
  private case class DoHeartBeat() extends Message

  case class StartTinkering(tinker: Tinker) extends Message

  //

  def apply(): Behavior[Message] = startingUp(None)

  private def startingUp(maybeClient: Option[UntypedActorRef]): Behavior[Message] =
    Behaviors.withTimers { timers =>
      timers.startTimerAtFixedRate(DoHeartBeat(), 50.seconds, 50.seconds)
      Behaviors.receive { (context, message) =>
        message match {
          case StartTinkering(tinker) =>
            // FIXME: HACK HACK HACK
//            implicit val hack: EnhancedTinker[MyCentralCast] = tinker match {
//              case enhanced: EnhancedTinker[MyCentralCast] => enhanced
//              case _ => ???
//            }

            maybeClient match {
              case Some(client) =>
                ready(client)(tinker)
              case None =>
                awaitingClient()(tinker)
            }
          case RegisterClient(client) =>
            context.log.info(s"Registered client $client")
            startingUp(Some(client))

          case DoHeartBeat() =>
            for (client <- maybeClient) {
              client ! TextMessage("heartbeat")
            }
            Behaviors.same

          case other =>
            context.log.warn(s"[startingUp] Ignoring $other, maybeClient=$maybeClient")
            Behaviors.same
        }
      }
    }

  private def awaitingClient()(implicit Tinker: Tinker/*EnhancedTinker[MyCentralCast]*/): Ability[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage {
      case RegisterClient(client) =>
        context.log.info(s"[awaitingClient] Got client: $client")
        ready(client)

      case IncomingMessage(message) =>
        message match {
          case message: TextMessage => context.log.warn(s"[awaitingClient] Ignoring text: $message")
          case message: BinaryMessage => context.log.warn(s"[awaitingClient] Ignoring binary ${message.getStrictData.size} bytes")
        }
        Behaviors.same

      case command: Command =>
        context.log.warn(s"[awaitingClient] Ignoring $command")
        Behaviors.same

      case DoHeartBeat() =>
        context.log.info("[awaitingClient] Ignoring heart beat, should not have happened yet")
        Behaviors.same

      case StartTinkering(_) => Behaviors.same
    }
  }

  private def ready(client: UntypedActorRef)(implicit Tinker: Tinker/*EnhancedTinker[MyCentralCast]*/): Ability[Message] = Tinker.setup { context =>
    context.actorContext.log.info(s"Ready with client $client")
    Behaviors.receiveMessage {
      case RegisterClient(newClient) =>
        context.actorContext.log.info(s"[ready] Switching to client: $newClient (dropping $client)")
        ready(newClient)

      case IncomingMessage(message) =>
        message match {
          case TextMessage.Strict("start") =>
            buffering(context.system.clock.now(), client, Nil)

          case message: BinaryMessage =>
            context.actorContext.log.warn(s"[ready] Ignoring binary ${message.getStrictData.size} bytes, not expecting binary data right now")
            Behaviors.same

          case other =>
            context.actorContext.log.warn(s"[ready] Unexpected: $other")
            Behaviors.same
        }

      case command: Command =>
        context.actorContext.log.warn(s"[ready] Ignoring $command")
        Behaviors.same

      case DoHeartBeat() =>
        client ! TextMessage("heartbeat")
        Behaviors.same

      case StartTinkering(_) => Behaviors.same
    }
  }

  private def buffering(startTime: ZonedDateTime, client: UntypedActorRef, buffer: List[ByteString])(implicit Tinker: Tinker/*EnhancedTinker[MyCentralCast]*/): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[Message] = context
    Behaviors.receiveMessage {
      case RegisterClient(client) =>
        context.actorContext.log.info(s"[buffering] Got client: $client")
        ready(client)

      case IncomingMessage(message) =>
        message match {
          case TextMessage.Strict("stop" | "completed") =>
            val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")
            val filename = s"mobile_audio_capture_${formatter.format(startTime)}.wav"
            // FIXME HACK HACK HCAK
            val contents = buffer.reverse.flatten.toArray
            context.actorContext.log.info(s"Writing ${contents.length} to disk")
            context.actorContext.log.warn(s"HACK HACK HACK - writing to disk somewhere weird!")
            writeWavWithJavax(filename, contents)
            ready(client)
          case message: BinaryMessage =>

            message match {
              case BinaryMessage.Strict(data) =>
                context.actorContext.log.info(s"[buffering] Adding ${message.getStrictData.size} bytes to the buffer")
                buffering(startTime, client, message.getStrictData :: buffer)
              case BinaryMessage.Streamed(dataStream) =>
                context.actorContext.log.info(s"[buffering] Not sure what to do with this data stream, ignoring: $dataStream")
                Behaviors.same
            }

          case other =>
            context.actorContext.log.warn(s"[buffering] Ignoring: $other")
            Behaviors.same
        }

      case command: Command =>
        context.actorContext.log.warn(s"[buffering] Ignoring $command")
        Behaviors.same

      case DoHeartBeat() =>
        client ! TextMessage("heartbeat")
        Behaviors.same

      case StartTinkering(_) => Behaviors.same
    }
  }

  //


  // FIXME it sounds a little funky?
  private def writeWavWithJavax(path: String, floatBytes: Array[Byte], sampleRate: Float = 44100f): Unit = {
    val format = new AudioFormat(
      AudioFormat.Encoding.PCM_FLOAT, // or PCM_SIGNED if you convert to 16-bit
      sampleRate,
      32,   // sample size in bits
      1,    // channels
      4,    // frame size (4 bytes per sample)
      sampleRate,
      false // little endian
    )

    val bais = new ByteArrayInputStream(floatBytes.toArray)
    val ais = new AudioInputStream(bais, format, floatBytes.length / 4)

    // FIXME: HACK HACK HACK hard-coded path
    val dir = Path("/Users/micseydel/obsidian_vaults/deliberate_knowledge_accretion/deliberate_knowledge_accretion_attachments/mobile_audio_captures")

    AudioSystem.write(ais, AudioFileFormat.Type.WAVE, new File(dir.resolve(path).toString()))
  }

  def html(host: String): String =
    s"""<!DOCTYPE html>
       |<html>
       |<head>
       |  <title>Recording App</title>
       |  <style>
       |    body {
       |      margin: 0;
       |      padding: 0;
       |      width: 100%;
       |      height: 100vh;
       |      display: flex;
       |      justify-content: center;
       |      align-items: center;
       |    }
       |    button {
       |      width: 80%;
       |      height: 80vh;
       |      font-size: 24px;
       |      border: none;
       |      border-radius: 10px;
       |      background-color: #4CAF50;
       |      color: #fff;
       |      cursor: pointer;
       |    }
       |  </style>
       |</head>
       |<body>
       |  <button id="record-button">Record</button>
       |
       |  <script>
       |    const recordButton = document.getElementById('record-button');
       |    let recording = false;
       |
       |    navigator.mediaDevices.getUserMedia({ audio: true })
       |      .then(stream => {
       |        const audioContext = new AudioContext();
       |        const source = audioContext.createMediaStreamSource(stream);
       |        const gainNode = audioContext.createGain();
       |        const scriptProcessorNode = audioContext.createScriptProcessor(4096, 1, 1);
       |
       |        let recording = false;
       |        const socket = new WebSocket('$host');
       |        console.log('Created socket');
       |
       |        function startRecording() {
       |          if (!recording) {
       |            console.log('Recording!');
       |            recording = true;
       |            recordButton.textContent = 'Stop';
       |            gainNode.gain.value = 1; // Enable audio streaming
       |            source.connect(scriptProcessorNode);
       |            scriptProcessorNode.connect(gainNode);
       |
       |            scriptProcessorNode.onaudioprocess = (event) => {
       |              console.log('Audio event!');
       |              const audioData = event.inputBuffer.getChannelData(0);
       |              const audioChunk = new Float32Array(audioData.length);
       |              for (let i = 0; i < audioData.length; i++) {
       |                audioChunk[i] = audioData[i];
       |              }
       |              socket.send(audioChunk.buffer);
       |            };
       |
       |            socket.send('start');
       |          } else {
       |            stopRecording();
       |          }
       |        }
       |
       |        function stopRecording() {
       |          console.log('stopping recording');
       |          recording = false;
       |          recordButton.textContent = 'Record';
       |          gainNode.gain.value = 0; // Disable audio streaming
       |          source.disconnect(scriptProcessorNode);
       |          scriptProcessorNode.disconnect(gainNode);
       |
       |          socket.send('stop');
       |        }
       |
       |        recordButton.addEventListener('touchstart', startRecording);
       |        recordButton.addEventListener('mousedown', startRecording);
       |      })
       |      .catch(error => {
       |        console.error('Error setting up audio streaming:', error);
       |      });
       |  </script>
       |</body>
       |</html>""".stripMargin
}
