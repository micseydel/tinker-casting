package me.micseydel.actor

import me.micseydel.NoOp
import me.micseydel.actor.perimeter.AranetActor
import me.micseydel.actor.perimeter.AranetActor.Aranet
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Operator, Tinker, TinkerColor, TinkerContext}

import scala.util.{Failure, Success}

object HumidityWatcherActor {
  sealed trait Message

  private case class ReceiveAranet(result: AranetActor.Result) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Humidity Watcher", TinkerColor.random(), "🌵") { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    context.system.operator !! Operator.SubscribeAranet4(context.messageAdapter(ReceiveAranet))

    Tinker.receiveMessage {
      case ReceiveAranet(result) =>
        result match {
          case AranetActor.AranetFailure(throwable) => throw throwable
          case AranetActor.AranetResults(aras, meta) =>
            val lines = s"- generated at ${meta.captureTime}" :: aras.map {
              case Aranet(address, co2, humidity, name, pressure, rssi, temperature) =>
                s"- $name = $humidity"
            }

            val markdown = lines.mkString("\n")
            noteRef.setMarkdown(markdown) match {
              case Failure(exception) => throw exception
              case Success(NoOp) =>
            }
        }

        Tinker.steadily
    }
  }
}
