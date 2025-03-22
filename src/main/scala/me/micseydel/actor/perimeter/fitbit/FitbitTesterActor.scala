package me.micseydel.actor.perimeter.fitbit

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.Common
import me.micseydel.actor.perimeter.fitbit.FetcherUtil.{ActivitiesHeartIntraday, ActivitiesHeartSummary, ActivitiesHeartValue, DataPoint, FitbitActiveTimes, FitbitActiveTimesIntraday, FitbitHeartRate, FitbitMinute, FitbitMinuteValue, HeartRateZone}
import me.micseydel.actor.perimeter.fitbit.FitbitModel.{FitbitSteps, StepsAt, StepsDataSet, StepsSummary}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef
import spray.json.DefaultJsonProtocol.StringJsonFormat
import spray.json._

import scala.util.{Failure, Success, Try}

object FitbitTesterActor {
  sealed trait Message

  private case class ReceiveSteps(payload: FitbitSteps) extends Message

  private case class ReceiveHeartRate(payload: FitbitHeartRate) extends Message

  private case class ReceiveCalories(payload: String) extends Message

  private case class ReceiveActivities(payload: String) extends Message

  private case class ReceiveActiveTimes(payload: String) extends Message

  //

  def apply(fitbit: SpiritRef[FitbitActor.Message])(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message]("Fitbit API Testing", TinkerColor.random(), "👨‍🔬") { (context, noteRef) =>
    implicit val tc: TinkerContext[_] = context

    val today = context.system.clock.today()

    /**
     * Dev hack for the adapter not working until I have the static typing (and usually parsing) setup.
     * Adapters are cheap, just a table lookup, whereas these are actors that run the context within a thread.
     */
    def createProxy[T](f: String => T, target: SpiritRef[T]): Behavior[String] =
      Behaviors.receiveMessage { message =>
        target.underlying ! f(message)
        Behaviors.same
      }

    // context.messageAdapter doesn't work, because Akka treats all the string-accepting adapters as the same
    val caloriesProxy = context.cast(createProxy(ReceiveCalories, context.self), "calories-proxy")
    val activitiesProxy = context.cast(createProxy(ReceiveActivities, context.self), "activities-proxy")
    val activeTimesProxy = context.cast(createProxy(ReceiveActiveTimes, context.self), "activetimes-proxy")

    fitbit !! FitbitActor.RequestSteps(context.messageAdapter(ReceiveSteps), today)
    fitbit !! FitbitActor.RequestHeartRate(context.messageAdapter(ReceiveHeartRate), today)
    fitbit !! FitbitActor.RequestCalories(caloriesProxy, today)
    fitbit !! FitbitActor.RequestActivities(activitiesProxy, today)
    fitbit !! FitbitActor.RequestActiveTimes(activeTimesProxy, today)

    implicit val nr: NoteRef = noteRef

    behavior(State())
  }

  private def behavior(state: State)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    state match {
      case State(stepsPayload, heartRatePayload, caloriesPayload, activitiesPayload, activeTimesPayload) =>
        val formattedSteps = stepsPayload.map {
          case FitbitSteps(activitiesSteps, StepsDataSet(dataset, datasetInterval, datasetType)) =>
            val summary = activitiesSteps match {
              case List(StepsSummary(dateTime, value)) => s"$value steps on $dateTime ($datasetInterval $datasetType interval, 0s excluded)"
              case other => s"Non-singular summary: $other"
            }

            dataset.collect {
              case StepsAt(time, value) if value > 0 =>
                s"    - $time: $value"
            }.mkString(s"- $summary\n", "\n", "")
        }.getOrElse("none")

        val formattedHeartRate = heartRatePayload.collect {
          case FitbitHeartRate(List(ActivitiesHeartSummary(dateTime, ActivitiesHeartValue(restingHeartRate, heartRateZones))), ActivitiesHeartIntraday(dataset, datasetInterval, datasetType)) =>
            val list = dataset.map {
              case DataPoint(time, value) =>
                s"    - $time $value"
            }.mkString("\n")

            val formattedHeartRateZones = heartRateZones.collect {
              case HeartRateZone(caloriesOut, max, min, minutes, name) if minutes > 0 =>
                s"    - $name: expended ~${caloriesOut.toInt} calories over $minutes minutes in range \\[$min, $max]"
            }.mkString("\n")

            s"- Resting heart rate $restingHeartRate for $dateTime\n- Zones:\n$formattedHeartRateZones\n- Data points:\n$list"
        }.getOrElse("none")

        import spray.json._
        implicit val jsonFormat: RootJsonFormat[FitbitActiveTimesIntraday] = me.micseydel.actor.perimeter.fitbit.FetcherUtil.FitbitActiveTimesJsonFormat.FitbitActiveTimesIntradayJsonFormat
        val formattedActiveTimes = Try(activeTimesPayload.parseJson.convertTo[FitbitActiveTimesIntraday]) match {
          case Failure(exception) => s"""\n```\n${Common.getStackTraceString(exception)}\n```"""
          case Success(FitbitActiveTimesIntraday(List(FitbitActiveTimes(dateTime, minutes)))) =>
            val formattedMinutes = minutes.collect {
              case FitbitMinute(minute, FitbitMinuteValue(activeZoneMinutes)) if activeZoneMinutes > 0 =>
                s"    - $minute $activeZoneMinutes"
            }.mkString("\n")
            s"- $dateTime\n$formattedMinutes"
          case Success(FitbitActiveTimesIntraday(list)) =>
            s"- unexpected $list"
        }

        noteRef.setMarkdown(
          s"""- Generated ${context.system.clock.now()}
             |
             |# Active Times Payload
             |
             |## Formatted
             |
             |$formattedActiveTimes
             |
             |## Raw
             |
             |```
             |$activeTimesPayload
             |```
             |
             |# Heart Rate
             |
             |$formattedHeartRate
             |
             |# Steps
             |
             |$formattedSteps
             |
             |---
             |
             |# Calories Payload
             |
             |```
             |$caloriesPayload
             |```
             |
             |# Activities Payload
             |
             |```
             |$activitiesPayload
             |```
             |""".stripMargin)
    }

    Tinker.receiveMessage { message =>
      context.actorContext.log.info(s"Processing message $message")
      message match {
        case ReceiveSteps(payload) => behavior(state.copy(steps = Some(payload)))
        case ReceiveHeartRate(payload) => behavior(state.copy(heartRatePayload = Some(payload)))
        case ReceiveCalories(payload) => behavior(state.copy(caloriesPayload = payload))
        case ReceiveActivities(payload) => behavior(state.copy(activitiesPayload = payload))
        case ReceiveActiveTimes(payload) => behavior(state.copy(activeTimesPayload = payload))
      }
    }
  }

  private case class State(
                            steps: Option[FitbitSteps] = None,
                            heartRatePayload: Option[FitbitHeartRate] = None,
                            caloriesPayload: String = "-",
                            activitiesPayload: String = "-",
                            activeTimesPayload: String = "-"
                          )
}
