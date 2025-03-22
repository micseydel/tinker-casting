package me.micseydel.actor.perimeter.fitbit

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.actor.perimeter.fitbit.FitbitModel.FitbitSteps
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.vault.persistence.NoteRef

object FitbitTesterActor {
  sealed trait Message

  private case class ReceiveSteps(payload: FitbitSteps) extends Message

  private case class ReceiveHeartRate(payload: String) extends Message

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
    // this is a simple dev hack,
    val hrProxy = context.cast(createProxy(ReceiveHeartRate, context.self), "hr-proxy")
    val caloriesProxy = context.cast(createProxy(ReceiveCalories, context.self), "calories-proxy")
    val activitiesProxy = context.cast(createProxy(ReceiveActivities, context.self), "activities-proxy")
    val activeTimesProxy = context.cast(createProxy(ReceiveActiveTimes, context.self), "activetimes-proxy")

    fitbit !! FitbitActor.RequestSteps(context.messageAdapter(ReceiveSteps), today)
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

        import me.micseydel.actor.perimeter.fitbit.FitbitModel.StepsJsonFormat.fitbitStepsFormat
        import spray.json._

        val formattedSteps = stepsPayload.map(_.toJson).getOrElse("none")

        noteRef.setMarkdown(
          s"""- Generated ${context.system.clock.now()}
             |# Steps Payload
             |
             |```
             |$formattedSteps
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
        case ReceiveSteps(payload) => behavior(state.copy(steps = Some(payload)))
        case ReceiveHeartRate(payload) => behavior(state.copy(heartRatePayload = payload))
        case ReceiveCalories(payload) => behavior(state.copy(caloriesPayload = payload))
        case ReceiveActivities(payload) => behavior(state.copy(activitiesPayload = payload))
        case ReceiveActiveTimes(payload) => behavior(state.copy(activeTimesPayload = payload))
      }
    }
  }

  private case class State(
                            steps: Option[FitbitSteps] = None,
                            heartRatePayload: String = "-",
                            caloriesPayload: String = "-",
                            activitiesPayload: String = "-",
                            activeTimesPayload: String = "-"
                          )
}
