package me.micseydel.actor.inactive

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import me.micseydel.model.RawSensorData

object PurpleAirActor {
  sealed trait Message
  private case class ReceiveRawSensorData(data: RawSensorData) extends Message

  // FIXME: goal - log `curl -s '192.168.50.146/json?live=true' | jq '.["pm2.5_aqi"]'` calls
  // note that http://192.168.50.146/?live=true can be interesting too

  def apply(): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.same
  }
}
