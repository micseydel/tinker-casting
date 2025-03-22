package me.micseydel.actor.perimeter.fitbit

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.perimeter.fitbit.FetcherUtil.FitbitSteps
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success, Try}

  object FitbitTesterActor {
    sealed trait Message

    private case class ReceiveSteps(payload: String) extends Message

    private case class ReceiveHeartRate(payload: String) extends Message

    private case class ReceiveCalories(payload: String) extends Message

    private case class ReceiveActivities(payload: String) extends Message

    private case class ReceiveActiveTimes(payload: String) extends Message

    //

    private def createProxy[T](f: String => T, target: SpiritRef[T]): Behavior[String] =
      Behaviors.receiveMessage { message =>
        target.underlying ! f(message)
        Behaviors.same
      }

    //

    def apply(fitbit: SpiritRef[FitbitActor.Message])(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message]("Fitbit API Testing", TinkerColor.random(), "👨‍🔬") { (context, noteRef) =>
      implicit val tc: TinkerContext[_] = context

      val today = context.system.clock.today()

      // context.messageAdapter doesn't work, because Akka treats all the string-accepting adapters as the same
      val stepsProxy = context.cast(createProxy(ReceiveSteps, context.self), "steps-proxy")
      val hrProxy = context.cast(createProxy(ReceiveHeartRate, context.self), "hr-proxy")
      val caloriesProxy = context.cast(createProxy(ReceiveCalories, context.self), "calories-proxy")
      val activitiesProxy = context.cast(createProxy(ReceiveActivities, context.self), "activities-proxy")
      val activeTimesProxy = context.cast(createProxy(ReceiveActiveTimes, context.self), "activetimes-proxy")

      // FIXME: these all seem to be use the same adapter...
      //  hypothesis: Akka is treating all these messages as the same due to the signature
      fitbit !! FitbitActor.RequestSteps(stepsProxy, today)
      fitbit !! FitbitActor.RequestHeartRate(hrProxy, today)
      fitbit !! FitbitActor.RequestCalories(caloriesProxy, today)
      fitbit !! FitbitActor.RequestActivities(activitiesProxy, today)
      fitbit !! FitbitActor.RequestActiveTimes(activeTimesProxy, today)

      implicit val nr: NoteRef = noteRef

      behavior(State())
    }

    private def behavior(state: State)(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
      state match {
        case State(stepsPayload, heartRatePayload, caloriesPayload, activitiesPayload, activeTimesPayload) =>

          import me.micseydel.actor.perimeter.fitbit.FetcherUtil.StepsJsonFormat.fitbitStepsFormat
          import spray.json._

          val flag = if (stepsPayload != "-") {
            val t: Try[FitbitSteps] = Try(stepsPayload.parseJson.convertTo[FitbitSteps])

            t match {
              case Failure(exception) =>
                context.actorContext.log.warn("steps extraction failed", exception)
                "see log"
              case Success(_) =>
                "success!"
            }

          } else {
            "🤷"
          }


          noteRef.setMarkdown(
            s"""- Generated ${context.system.clock.now()}
               |# Steps Payload
               |
               |json processing works? $flag
               |
               |```
               |$stepsPayload
               |```
               |
               |# Heart Rate Payload
               |
               |```
               |$heartRatePayload
               |```
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
               |
               |# Active Times Payload
               |
               |```
               |$activeTimesPayload
               |```
               |""".stripMargin)
      }

      Tinker.receiveMessage { message =>
        context.actorContext.log.info(s"Processing message $message")
        message match {
          case ReceiveSteps(payload) => behavior(state.copy(stepsPayload = payload))
          case ReceiveHeartRate(payload) => behavior(state.copy(heartRatePayload = payload))
          case ReceiveCalories(payload) => behavior(state.copy(caloriesPayload = payload))
          case ReceiveActivities(payload) => behavior(state.copy(activitiesPayload = payload))
          case ReceiveActiveTimes(payload) => behavior(state.copy(activeTimesPayload = payload))
        }
      }
    }

    private case class State(
      stepsPayload: String = "-",
      heartRatePayload: String = "-",
      caloriesPayload: String = "-",
      activitiesPayload: String = "-",
      activeTimesPayload: String = "-"
    )
  }
