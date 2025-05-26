package me.micseydel.actor

import cats.data.NonEmptyList
import me.micseydel.actor.DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown
import me.micseydel.actor.FrustrationListener.{DistressDetected, DistressDetection}
import me.micseydel.actor.Halto._
import me.micseydel.actor.HungerTracker.HungerState
import me.micseydel.actor.NutritionListener.LastAte
import me.micseydel.actor.inactive.fitbit.FitbitActor
import me.micseydel.actor.notifications.NotificationCenterManager.{JustSideEffect, PushNotification}
import me.micseydel.actor.perimeter.AranetActor.{Aranet, AranetFailure, AranetResults, Meta}
import me.micseydel.actor.perimeter.{AranetActor, HueControl}
import me.micseydel.actor.inactive.fitbit.FitbitModel.{SleepReport, SleepSummary}
import me.micseydel.app.AppConfiguration.NtfyKeys
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.{Yellow, rgb}
import me.micseydel.dsl._
import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat
import me.micseydel.util.TimeUtil
import org.slf4j.Logger
import spray.json.{DefaultJsonProtocol, DeserializationException, JsObject, JsString, JsValue, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime
import scala.annotation.{tailrec, unused}
import scala.concurrent.duration.DurationInt
import scala.util.Random

object Halto {

  // mailbox

  sealed trait Event {
    def eventTime: ZonedDateTime
  }

  case class ReceiveHungerState(state: HungerState) extends Event {
//    override def eventTime: ZonedDateTime = state.lastAte.at
    // FIXME HACK: this was resulting in no updates on a day where I didn't eat
    override def eventTime: ZonedDateTime = ZonedDateTime.now()
  }

  case class ReceiveFrustrationDetected(detected: FrustrationListener.DistressDetected) extends Event {
    override def eventTime: ZonedDateTime = detected.notedTranscription.capture.captureTime
  }

  case class ReceiveSleepReport(sleepReport: SleepReport) extends Event {
    override def eventTime: ZonedDateTime = sleepReport.sleep.map(_.endTime).max
  }

  case class ReceiveAranetResult(aranetResult: AranetActor.Result) extends Event {
    override def eventTime: ZonedDateTime = aranetResult.getOrFail.meta.captureTime
  }

  // for pattern matching

  private val Hungry = Some(true)
  private val NotHungry = Some(false)
  private val Tired = Some(true)
  private val NotTired = Some(false)
  private val HighCO2 = Some(true)
  private val NotHighCO2 = Some(false)

  // behaviors

//  def apply(fitbitActor: SpiritRef[FitbitActor.Message], ntfyKeys: NtfyKeys)(implicit Tinker: Tinker): Ability[Event] = Tinkerer(Yellow, "🛑").setup { context =>
//    implicit val c: TinkerContext[Event] = context
//
//    c.actorContext.log.info("Starting hunger tracker, frustration listener, sleep report actor and subscribing to CO2")
//
//    // HALT - Hungry, Angry (frustrated), (Lonely), Tired
//
////    @unused // internally driven
////    val hungerTrackerActor: SpiritRef[HungerTracker.Message] = context.cast(HungerTracker(context.messageAdapter(ReceiveHungerState), ntfyKeys.foodTime), "HungerTracker")
////
////    @unused // frustrationListener notifies halto when frustrated transcriptions are detected
////    val frustrationListener: SpiritRef[FrustrationListener.Event] = context.cast(FrustrationListener(context.messageAdapter(ReceiveFrustrationDetected)), "DistressListener")
//
//    // FIXME: lonely could integrate Google Calendar to estimate?
//
////    @unused // sleep report actor is driven by an internal timer
////    val sleepReportActor: SpiritRef[SleepReportActor.Message] = context.cast(SleepReportActor(fitbitActor, context.messageAdapter(ReceiveSleepReport)), "SleepReportActor")
//
//    context.system.operator !! Operator.SubscribeAranet4(context.messageAdapter(ReceiveAranetResult))
//
//    if (ntfyKeys.searchSpace.isEmpty) {
//      context.actorContext.log.warn(s"Search space key not configured, will not be sending push notifications with Ntfy")
//    }
//
//    val log = context.actorContext.log
//    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[Event]]] = context.cast(DailyNotesRouter(
//      "HALT-O notes",
//      "halto_notes",
//      MessageListJsonProtocol.messageJsonFormat,
//      toMarkdown(_: List[Event], _: TinkerClock)(log)
//    ), "HALTODailyNotesRouter")
//
//    dailyNotesAssistant !! DailyNotesRouter.Envelope(DailyMarkdownFromPersistedMessagesActor.RegenerateMarkdown(), context.system.clock.now())
//
//    behavior(HaltState.Default, None)(Tinker, hungerTrackerActor, dailyNotesAssistant, ntfyKeys.searchSpace)
//  }

  private def behavior(state: HaltState, frustrationPushCooldownUntil: Option[ZonedDateTime])(implicit Tinker: Tinker, hungerTrackerActor: SpiritRef[HungerTracker.Message], dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[Event]]], searchSpaceNtfyKey: Option[String]): Ability[Event] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[Event] = context
    implicit val tc: TinkerClock = context.system.clock

    dailyNotesAssistant !! DailyNotesRouter.Envelope(StoreAndRegenerateMarkdown(message), message.eventTime)

    context.actorContext.log.info(s"Received a ${message.getClass.getCanonicalName}, already forwarded to ${dailyNotesAssistant.path}")

    val maybeNewCooldownUntil: Option[ZonedDateTime] = message match {
      case ReceiveHungerState(HungerState(lastAte, isHungry)) =>
        context.actorContext.log.info(s"""Last ate $lastAte, currently ${if (isHungry) "" else "NOT "} hungry""")
        None

      case ReceiveFrustrationDetected(DistressDetected(notedTranscription, specifics)) =>
        // FIXME: REVIEW
        context.actorContext.log.info(s"Doing a light show because distress was detected, also sending a push notification")
//        context.system.hueControl !! HueControl.DoALightShow()

        // focus on binary thinking first
        // otherwise fallback to random
        // ...which intelligently excludes food if I've eaten recently
        val messageText = {
          if (containsBinaryThinking(notedTranscription.capture.whisperResult.whisperResultContent.text.toLowerCase)) {
            "What [[binary thinking]] is happening?"
          } else {
            val possibilities = NonEmptyList.of(
              // FIXME: "Pause: Name the search space in which you are frustrated."
              "What's the current [[search space]]? Name it 👀",
//              "Does a [[Choice Point]] need expanding? 🌱",
              "What are the current [[reference frames]]? 🖼️",
              "Consider creating a [[choice point]] note ⚖️",
              "Is this a good time for a walk? 🚶",
              "Is this a time for [[Hypotheses to test]]? 👩‍🔬"
            )

            state.lastAte.map(TimeUtil.timeSince) match {
              case Some(timeSince) if timeSince.toHours > 2 =>
                context.system.getRandomElement("Try #eating COCONUT OIL 🥥" :: possibilities)
              case _ =>
                context.system.getRandomElement(possibilities)
            }

            context.system.getRandomElement(possibilities)
          }
        }
        context.actorContext.log.info(s"Using push notification text: $messageText")

        val currentlyInCooldown = frustrationPushCooldownUntil.exists(context.system.clock.now().isBefore)
        if (!currentlyInCooldown) {
          // FIXME
          searchSpaceNtfyKey.foreach { key =>
            context.system.notifier !! newPushNotification(messageText, key)
          }
        } else {
          context.actorContext.log.info(s"Cooldown until $frustrationPushCooldownUntil")
        }

        Some(ZonedDateTime.now().plusMinutes(5))

      case ReceiveSleepReport(_) | ReceiveAranetResult(_) =>
        context.actorContext.log.info(s"Taking no immediate action for ${message.getClass.getCanonicalName}")
        None
    }

    val newFrustrationCooldown = (maybeNewCooldownUntil, frustrationPushCooldownUntil) match {
      case (None, None) => None
      case (None, old) => old
      case (fresh, None) => fresh
      case (updated, _) => updated
    }

    context.actorContext.log.info(s"Using new frustration cooldown $newFrustrationCooldown (from maybeNewCooldownUntil=$maybeNewCooldownUntil, frustrationPushCooldownUntil=$frustrationPushCooldownUntil)")

    state.integrate(message) match {
      case (false, _) =>
        context.actorContext.log.info(s"Integration of ${message.getClass.getCanonicalName} resulted in no update")
        Tinker.steadily
      case (true, updatedState) =>
        if (updatedState.level > 1 && updatedState.level > state.level) {
          val msg = stateToFriendlyString(updatedState)
          context.actorContext.log.info(s"Sending push notification: $msg")
          // FIXME
          searchSpaceNtfyKey.foreach { key =>
            context.system.notifier !! newPushNotification(msg, key)
          }
        } else {
          context.actorContext.log.info(s"Level ${updatedState.level}, not sending any push notification")
        }

        behavior(updatedState, newFrustrationCooldown)
    }
  }

  private def containsBinaryThinking(text: String): Boolean = {
    text.contains("always") ||
      text.contains("never") ||
      text.contains("nothing") ||
      text.contains("everything") ||
      text.contains("everytime") ||
      text.contains("i can't")
  }

  private def stateToFriendlyString(state: HaltState): String = {
    state match {
      case HaltState(Hungry, Tired, HighCO2, _, _) =>
        "STOP THE WORLD MOMENT - grab food and nap if possible, otherwise go outside or open up"

      case HaltState(Hungry, Tired, NotHighCO2, _, _) =>
        "Grab food and nap if possible"

      case HaltState(Hungry, NotTired, HighCO2, _, _) =>
        "Grab food and eat outside if possible"

      case HaltState(NotHungry, Tired, HighCO2, _, _) =>
        "Ventilate and nap if possible"

      case other =>
        s"none ($other)"
    }
  }

  private def newPushNotification(msg: String, searchSpaceKey: String): JustSideEffect = {
    // not in the notification center
    JustSideEffect(PushNotification(searchSpaceKey, msg))
  }


  // util

  final case class HaltState(hungry: Option[Boolean], tired: Option[Boolean], highCO2: Option[Boolean], lastAte: Option[ZonedDateTime], co2: Option[Int]) {
    private def bool2int(b: Boolean): Int = if (b) 1 else 0

    def level: Int = List(hungry, tired, highCO2).map(_.map(bool2int)).map(_.getOrElse(0)).sum

    def integrate(messages: List[Event])(implicit tinkerClock: TinkerClock): HaltState = {
      messages.foldRight(HaltState.Default) { (message, priorState) =>
        priorState.integrate(message)._2
      }
    }

    /**
     * @return (wasUpdated, state)
     */
    def integrate(message: Event)(implicit tinkerClock: TinkerClock): (Boolean, HaltState) = {

      message match {
        case ReceiveHungerState(HungerState(LastAte(lastAte), isHungryLatest)) =>
          val latestState = this.copy(hungry = Some(isHungryLatest), lastAte = Some(lastAte))
          if (this == latestState) {
            (false, this)
          } else {
            (true, latestState)
          }

        case ReceiveFrustrationDetected(DistressDetected(notedTranscription, specifics)) =>
          (false, this)

        case ReceiveSleepReport(sleepReport: SleepReport) =>
          // FIXME: this should not respond poorly to a prior day's report coming in
          val currentlySleepy = sleepReport.summary.totalMinutesAsleep < 6.hours.toMinutes
          if (!tired.contains(currentlySleepy)) {
            val updatedState = this.copy(tired = Some(currentlySleepy))
            (true, updatedState)
          } else {
            (false, this)
          }

        case ReceiveAranetResult(aranetResult) =>
          aranetResult match {
            case AranetActor.AranetFailure(_) => (false, this)
            case aranetResult@AranetResults(_, _) =>
              aranetResult.preferred match {
                case Some(Aranet(_, co2, humidity, _, pressure, _, temperature)) =>
                  val co2CurrentlyHigh = co2 > 1000
                  if (!highCO2.contains(co2CurrentlyHigh)) {
                    val updatedState = this.copy(highCO2 = Some(co2CurrentlyHigh), co2 = Some(co2))
                    (true, updatedState)
                  } else {
                    (false, this)
                  }
                case None =>
                  (false, this)
              }
          }

      }
    }
  }

  object HaltState {
    val Default: HaltState = HaltState(hungry = None, tired = None, highCO2 = None, lastAte = None, co2 = None)
  }

  private def toMarkdown(messages: List[Halto.Event],  tinkerClock: TinkerClock)(log: Logger): String = {
    MarkdownGenerator.toMarkdown(messages.reverse)(tinkerClock, log)
  }

  private object MessageListJsonProtocol extends DefaultJsonProtocol {

    import HungerTrackJsonProtocol.hungerStateJsonFormat

    private implicit val distressDetectionFormat: RootJsonFormat[DistressDetection] = jsonFormat2(DistressDetection)
    private implicit val distressDetectedFormat: RootJsonFormat[FrustrationListener.DistressDetected] = jsonFormat2(FrustrationListener.DistressDetected)
    implicit val receiveFrustrationDetectedFormat: RootJsonFormat[ReceiveFrustrationDetected] = jsonFormat1(ReceiveFrustrationDetected)

    import me.micseydel.actor.inactive.fitbit.FitbitModel.SleepJsonProtocol.sleepReportFormat

    implicit val receiveSleepReportFormat: RootJsonFormat[ReceiveSleepReport] = jsonFormat1(ReceiveSleepReport)

    import me.micseydel.actor.perimeter.AranetActor.AranetJsonProtocol.payloadFormat

    implicit object AranetFailureJsonFormat extends RootJsonFormat[AranetFailure] {
      def write(m: AranetFailure): JsValue = {
        JsString("AranetFailure")
      }

      def read(value: JsValue): AranetFailure = {
        AranetFailure(new RuntimeException("placeholder/sentinel"))
      }
    }

    implicit object AranetActorResultJsonFormat extends RootJsonFormat[AranetActor.Result] {
      def write(m: AranetActor.Result): JsValue = {
        val (jsObj, typ) = m match {
          case o: AranetFailure => (o.toJson.asJsObject, "AranetFailure")
          case o: AranetResults => (o.toJson.asJsObject, "AranetResults")
        }
        JsObject(jsObj.fields + ("type" -> JsString(typ)))
      }

      def read(value: JsValue): AranetActor.Result = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("AranetFailure")) => value.convertTo[AranetFailure]
          case Seq(JsString("AranetResults")) => value.convertTo[AranetResults]
          case other => throw DeserializationException(s"""Unknown type $other, expected {ReceiveLastAte, ReceiveFrustrationDetected, ReceiveSleepReport, ReceiveAranetResult}""")
        }
      }
    }

    implicit val receiveAranetResultFormat: RootJsonFormat[ReceiveAranetResult] = jsonFormat1(ReceiveAranetResult)

    implicit val receiveHungerStateFormat: RootJsonFormat[ReceiveHungerState] = jsonFormat1(ReceiveHungerState)

    implicit object messageJsonFormat extends RootJsonFormat[Event] {
      def write(m: Event): JsValue = {
        val (jsObj, typ) = m match {
          case o: ReceiveFrustrationDetected => (o.toJson.asJsObject, "ReceiveFrustrationDetected")
          case o: ReceiveSleepReport => (o.toJson.asJsObject, "ReceiveSleepReport")
          case o: ReceiveAranetResult => (o.toJson.asJsObject, "ReceiveAranetResult")
          case o: ReceiveHungerState => (o.toJson.asJsObject, "ReceiveHungerState")
        }
        JsObject(jsObj.fields + ("type" -> JsString(typ)))
      }

      def read(value: JsValue): Event = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("ReceiveFrustrationDetected")) => value.convertTo[ReceiveFrustrationDetected]
          case Seq(JsString("ReceiveSleepReport")) => value.convertTo[ReceiveSleepReport]
          case Seq(JsString("ReceiveAranetResult")) => value.convertTo[ReceiveAranetResult]
          case Seq(JsString("ReceiveHungerState")) => value.convertTo[ReceiveHungerState]
          case other => throw DeserializationException(s"""Unknown type $other, expected {ReceiveLastAte, ReceiveFrustrationDetected, ReceiveSleepReport, ReceiveAranetResult}""")
        }
      }
    }
  }
}

// FIXME: idea - generic batcher
// take a list and a function
// returns a list of wrapper objects which have either the originals or...
//

private object MarkdownGenerator {
  def toMarkdown(messages: List[Halto.Event])(implicit tinkerClock: TinkerClock, log: Logger): String = {
    // FIXME: this is an example of integration shrinking a search space - List[Message] => HaltState which is 3 bits (plus lastAte)
    val currentState: HaltState = HaltState.Default.integrate(messages)

    val allEvents: String = helper(messages, Nil).mkString("- ", "\n- ", "")

    def asEmoji(ob: Option[Boolean]): String = ob match {
      case Some(true) => "❌"
      case Some(false) => "✅"
      case None => "🤷"
    }

    val lastAteAt = currentState.lastAte
      .filter(_ => currentState.hungry.getOrElse(false))
      .map(TimeUtil.zonedDateTimeToHourMinWithinDay)
      .map(fmtd => s" ([[last_ate|last ate]] $fmtd)")
      .getOrElse("")

    val co2Val = currentState.co2
      .filter(_ => currentState.highCO2.getOrElse(true))
      .map(co2 => s" ($co2)")
      .getOrElse("")

    s"""# State
       |
       |- Hungry: ${asEmoji(currentState.hungry)}$lastAteAt
       |- Tired: ${asEmoji(currentState.tired)}${if (currentState.tired.getOrElse(true)) " ([[Sleep Report]])" else ""}
       |- High CO2: ${asEmoji(currentState.highCO2)}$co2Val
       |- DEBUG
       |    - Total level: ${currentState.level}
       |    - State: $currentState
       |
       |# Events
       |
       |$allEvents""".stripMargin
  }

  @tailrec
  private def helper(messages: List[Event], accumulator: List[String]): List[String] = {
    messages match {
      case Nil =>
        accumulator

      case ReceiveAranetResult(response) :: maybeBatch =>
        response match {
          case AranetFailure(_) => helper(maybeBatch, accumulator)
          case result@AranetResults(_, _) =>
            val (line, remaining) = ConsecutiveCO2Batcher.batchConsecutiveCO2(result, maybeBatch)
            helper(remaining, line :: accumulator)
        }

      case head :: tail =>
        helper(tail, messageToString(head) :: accumulator)
    }
  }

  private def messageToString(message: Event): String = {
    message match {
      case ReceiveHungerState(HungerState(lastAte, isHungry)) =>
        s"Last ate $lastAte hungry=$isHungry"
      case ReceiveFrustrationDetected(DistressDetected(notedTranscription, specifics)) =>
        val ref = notedTranscription.noteId.wikiLinkWithAlias("ref")
        s"Distress detected, ${specifics.size} specific references ($ref)"
      case ReceiveSleepReport(SleepReport(_, SleepSummary(totalMinutesAsleep, _, _))) =>
        val hours = totalMinutesAsleep / 60
        val minutes = totalMinutesAsleep % 60
        s"Sleep report, latest $hours hours and $minutes minutes"
      case ReceiveAranetResult(aranetResult) =>
        s"CO2 ${aranetResult.getOrFail.preferred.map(_.co2).orNull}"
    }
  }
}

private object ConsecutiveCO2Batcher {

  case class CO2Result(value: Int, captureTime: ZonedDateTime, deviceName: String)

  case class BatchResult(from: CO2Result, to: CO2Result, inBetween: Int, monotonicallyIncreasing: Boolean)

  // FIXME: need to unit-test this
  @tailrec
  private def helper(messages: List[Event], accumulator: Option[Either[CO2Result, BatchResult]]): (Option[Either[CO2Result, BatchResult]], List[Event]) = messages match {

      // FIXME: I should log these drops, but for now the note just says "Capture failed" or whatever
      case ReceiveAranetResult(aranetResult) :: remainder if aranetResult.getOrFail.preferred.isEmpty =>
        (accumulator, remainder)

      case ReceiveAranetResult(rs@AranetResults(_, Meta(_, captureTime))) :: remainder =>
        val updated = rs.preferred match {
          case None =>
            None
          case Some(Aranet(_, newCo2Value, _, deviceName, _, _, _)) =>
            val newCo2Result = CO2Result(newCo2Value, captureTime, deviceName)
            accumulator match {
              case None =>
                Some(Left(newCo2Result))
              case Some(Left(earlier)) =>
                Some(Right(BatchResult(earlier, newCo2Result, 0, monotonicallyIncreasing = newCo2Result.value > earlier.value)))
              case Some(Right(BatchResult(firstInBatch, discarding, numBetween, alreadyMonotonicallyIncreasing))) =>
                Some(Right(BatchResult(
                  from = firstInBatch,
                  to = newCo2Result,
                  inBetween = numBetween + 1,
                  monotonicallyIncreasing = alreadyMonotonicallyIncreasing & newCo2Value > discarding.value
                )))
            }
        }

        helper(remainder, updated)

      case remainder =>
        (accumulator, remainder)
    }


  def batchConsecutiveCO2(first: AranetResults, theRest: List[Event]): (String, List[Event]) = {
    val initial = first.preferred.map { aranet =>
      // left is a single result, right is a batch
      // ...wrapped in an option if no preferred value
      Left(CO2Result(aranet.co2, first.meta.captureTime, aranet.name))
    }

    helper(theRest, initial) match {
      case (None, remainder) =>
        ("Capture failed", remainder)
      case (Some(Left(CO2Result(co2, captureTime, deviceName))), remainder) =>
        val prettyTime = TimeUtil.zonedDateTimeToTimeWithinDay24Hour(captureTime)
        (s"Captured **$co2** ([[$deviceName|$prettyTime]])", remainder)
      case (Some(Right(BatchResult(CO2Result(fromValue, fromCaptureTime, fromDeviceName), CO2Result(toValue, toCaptureTime, toDeviceName), inBetween, monotonicallyIncreasing))), remainder) =>
        val prettyFromTime = TimeUtil.zonedDateTimeToTimeWithinDay24Hour(fromCaptureTime)
        val prettyToTime = TimeUtil.zonedDateTimeToTimeWithinDay24Hour(toCaptureTime)

        val increasedOverall = toValue > fromValue
        val increasingEmoji = (increasedOverall, monotonicallyIncreasing) match {
          case (false, false) => "↘️"
          case (false, true) => "↘️"
          case (true, false) => "📈"
          case (true, true) => "↗️"
        }

        (
          s"Captured **$fromValue** ([[$fromDeviceName|$prettyFromTime]]) " +
            s"and later **$toValue** ([[$toDeviceName|$prettyToTime]])" +
            (if (inBetween > 0) s" ($inBetween captures between $increasingEmoji)" else ""),
          remainder
        )
    }
  }
}
