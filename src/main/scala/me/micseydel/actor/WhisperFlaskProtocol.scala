package me.micseydel.actor

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import me.micseydel.actor.WhisperFlaskProtocol.{Enqueue, Message}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.vault.VaultPath
import spray.json.DefaultJsonProtocol._
import spray.json.enrichAny

import java.net.UnknownHostException
import java.nio.file.{Files, Path, Paths}
import java.util.Base64
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object WhisperFlaskProtocol {
  sealed trait Message
  case class Enqueue(vaultPath: String) extends Message
}

// FIXME verify local stuff works (on same-machine)
object PrimaryWhisperFlaskAmbassador {
  case class Config(
                     whisperHost: String,
                     whisperPort: Int,
                     eventReceiverHost: String,
                     eventReceiverPort: Int,
                   )

  def apply(config: Config)(implicit httpExecutionContext: ExecutionContext): Behavior[Message] = Behaviors.setup { context =>
    context.log.info("Setting up primary Flask connection, but not verifying")
    // FIXME: ideally this would do like a "hello" call to the Flask server 🤷
    Behaviors.receiveMessage {
      case Enqueue(vaultPath) =>
        import context.system
        doAsyncHttp(vaultPath, config)
        Behaviors.same
    }
  }

  private def doAsyncHttp(vaultPath: String, config: Config)(implicit httpExecutionContext: ExecutionContext, system: ActorSystem[Nothing]): Unit = {
    val payload = Map(
      "vault_path" -> vaultPath,
      "callback_url" -> f"http://${config.eventReceiverHost}:${config.eventReceiverPort}/event"
    )

    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(
        method = HttpMethods.POST,
        uri = s"http://${config.whisperHost}:${config.whisperPort}/enqueue/vault_path",
        entity = HttpEntity(ContentTypes.`application/json`, payload.toJson.toString)
      )
    )

    responseFuture.onComplete {
      case Success(res) =>
//        println(res)
        res.discardEntityBytes()
      case Failure(t) =>
        system.log.warn(s"HTTP call failed for ${config.whisperHost}", t)
    }
  }
}

object WhisperUploadActor {
  case class Config(
                     whisperHost: String,
                     eventReceiverHost: String,
                     eventReceiverPort: Int,
                     vaultRoot: VaultPath
                   )

  def apply(config: Config)(implicit httpExecutionContext: ExecutionContext): Ability[Message] = Behaviors.setup { context =>
    context.log.info("Setting up secondary flask server actor")
    Behaviors.receiveMessage {
      case Enqueue(vaultPath) =>
        doAsyncHttp(
          config.whisperHost,
          f"http://${config.eventReceiverHost}:${config.eventReceiverPort}/event",
          vaultPath,
          config.vaultRoot.path,
          ignoreUnknownHost = true
        )(httpExecutionContext, context.system)
        Behaviors.same
    }
  }

  private def doAsyncHttp(host: String, callbackUrl: String, vaultPath: String, vaultRoot: Path, ignoreUnknownHost: Boolean)(implicit httpExecutionContext: ExecutionContext, system: ActorSystem[Nothing]): Unit = {
    val payload = Map(
      "vault_path" -> vaultPath,
      "callback_url" -> callbackUrl,
      "encoded" -> {
        val pathStr = vaultRoot.resolve(vaultPath).toString
        system.log.debug(s"About to open $pathStr = vaultRoot.resolve(vaultPath) = ($vaultRoot).resolve($vaultPath)")
        val path = Paths.get(pathStr)
        val bytes = Files.readAllBytes(path)
        Base64.getEncoder.encodeToString(bytes)
      }
    )

    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(
        method = HttpMethods.POST,
        uri = s"http://$host/enqueue/upload",
        entity = HttpEntity(ContentTypes.`application/json`, payload.toJson.toString)
      )
    )

    responseFuture.onComplete {
      case Success(res) =>
//        println(res)
        res.discardEntityBytes()
      case Failure(t: akka.stream.StreamTcpException) if t.getCause.isInstanceOf[UnknownHostException] =>
        if (ignoreUnknownHost) {
          system.log.info(s"$host unavailable for transcription")
        } else {
          system.log.warn(s"Upload enqueue unexpectedly failed $host, $vaultPath", t)
        }
      case Failure(t) =>
        system.log.warn(s"Upload enqueue failed $host, $vaultPath", t)
    }
  }
}
