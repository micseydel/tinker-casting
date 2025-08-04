package me.micseydel.actor

import me.micseydel.actor.HungerTracker.HungerState
import me.micseydel.actor.NutritionListener.LastAte
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.annotation.unused

object HungerTracker {

  // mailbox

  sealed trait Message

  case class ReceiveLastAte(lastAte: LastAte) extends Message

  case class ReceiveHungerState(hungerState: HungerState) extends Message

  // outbox

  /**
   * Careful with serializing this...
   * @param isHungry depends on the time of sending!
   */
  final case class HungerState(lastAte: LastAte, isHungry: Boolean)

  def apply(hungerSubscriber: SpiritRef[HungerState], foodTimeNtfyKey: Option[String])(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinkerer(rgb(135, 206, 235), "🫢").setup { context =>
    implicit val c: TinkerContext[_] = context
    implicit val tc: TinkerClock = context.system.clock

    @unused // nutritionHelperActor notifies halto when last_ate transcriptions are detected
    val nutritionHelperActor: SpiritRef[NutritionListener.Message] = context.cast(NutritionListener(context.messageAdapter(ReceiveLastAte)), "NutritionHelper")
    val foodReminder: SpiritRef[FoodReminderActor.Message] = context.cast(FoodReminderActor(context.messageAdapter(ReceiveHungerState), foodTimeNtfyKey), "FoodReminder")

    Tinker.receiveMessage {
      case ReceiveLastAte(lastAte) =>
        context.actorContext.log.info(s"Forwarding last ate to ${foodReminder.path}, hunger state to ${hungerSubscriber.path}")
        foodReminder !! FoodReminderActor.CaloriesConsumed(lastAte.at)
        hungerSubscriber !! HungerState(lastAte, lastAte.isHungry())
        Tinker.steadily

      case ReceiveHungerState(hs) =>
        context.actorContext.log.info(s"Forwarding hunger state to ${hungerSubscriber.path}")
        hungerSubscriber !! hs
        Tinker.steadily
    }
  }
}

object HungerTrackJsonProtocol extends DefaultJsonProtocol {

  import me.micseydel.util.JsonUtil.ZonedDateTimeJsonFormat
  private implicit val lastAteFormat: RootJsonFormat[LastAte] = jsonFormat1(LastAte)
  implicit val hungerStateJsonFormat: RootJsonFormat[HungerState] = jsonFormat2(HungerState)
}
