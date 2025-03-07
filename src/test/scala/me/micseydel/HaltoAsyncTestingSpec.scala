package me.micseydel

import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.{ActorRef, Scheduler}
import me.micseydel.actor.FrustrationListener.DistressDetected
import me.micseydel.actor.Halto
import me.micseydel.actor.notifications.NotificationCenterManager
import me.micseydel.actor.notifications.NotificationCenterManager.Chime
import me.micseydel.actor.perimeter.AranetActor.{Aranet, AranetResults}
import me.micseydel.actor.perimeter.fitbit.FitbitActor
import me.micseydel.actor.perimeter.fitbit.FitbitModel.{LevelsData, SleepEntry, SleepReport, SleepSummary}
import me.micseydel.actor.perimeter.{AranetActor, HueControl}
import me.micseydel.app.AppConfiguration.NtfyKeys
import me.micseydel.dsl.TinkerSystem
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.model._
import me.micseydel.testsupport.TinkerProbeActor.RichTinkerProbeActor
import me.micseydel.testsupport.{JsonRefForTesting, NoteRefWatcherHelper, SpiritTestKit, TestHelpers, TestTinkerContainer, TinkerProbeActor, TinkerTest, VirtualNoteRef}
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.{JsonRef, NoteRef}

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationInt

class HaltoAsyncTestingSpec extends TestTinkerContainer {
  val SearchSpaceKey = "searchspace"

  "Halto" must {
    "produce Markdown in a neutral state" in {
      haltoTestBasics(maybeLastAte = None, transcriptionToBroadcast = None) { (tinkerSystem, todaysHaltoNoteRef, _, probe) =>
        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler) should startWith(
          """# State
            |
            |- Hungry: 🤷
            |- Tired: 🤷 ([[Sleep Report]])
            |- High CO2: 🤷""".stripMargin)
        // debug may follow in the markdown, we don't care about that

        implicit val s: Scheduler = tinkerSystem.actorSystem.scheduler
        probe.getSideEffects() should be(Nil)
      }
    }

    "produce hunger-satisifed Markdown in an otherwise neutral state" in {
      val lastAte = Some(ZonedDateTime.now.minusHours(6))
      val transcriptionToBroadcast = Some(TestHelpers.simpleNotedTranscription("I just ate"))
      //        halto ! Halto.ReceiveHungerState(HungerState(LastAte(ZonedDateTime.now().minusHours(6)), isHungry = true)) FIXME
      haltoTestBasics(lastAte, transcriptionToBroadcast) { (tinkerSystem, todaysHaltoNoteRef, _, probe) =>
        // ignore first write, before hunger message
        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler)

        // make sure the hunger message had time to propagate FIXME may be better way to do this
        Thread.sleep(100)

        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler) should startWith(
          """# State
            |
            |- Hungry: ✅
            |- Tired: 🤷 ([[Sleep Report]])
            |- High CO2: 🤷""".stripMargin)
        // debug may follow in the markdown, we don't care about that

        implicit val s: Scheduler = tinkerSystem.actorSystem.scheduler
        probe.getSideEffects() should be(Nil)
      }
    }

    "be able to indicate satisfactory CO2" in {
      haltoTestBasics(maybeLastAte = None, transcriptionToBroadcast = None) { (tinkerSystem, todaysHaltoNoteRef, halto, probe) =>
        // ignore first write, before hunger message
//        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler)

        //      halto ! Halto.ReceiveSleepReport(SleepReport())
        val aranet = Aranet(address = "", co2 = 0, humidity = 0, name = "1A300", pressure = 0, rssi = 0, temperature = 0)
        halto ! Halto.ReceiveAranetResult(AranetResults(List(aranet), AranetActor.Meta(0, ZonedDateTime.now())))
        //      halto ! Halto.ReceiveFrustrationDetected(DistressDetected())

        // make sure the message had time to propagate FIXME may be better way to do this
        Thread.sleep(100)

        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler) should startWith(
          """# State
            |
            |- Hungry: 🤷
            |- Tired: 🤷 ([[Sleep Report]])
            |- High CO2: ✅""".stripMargin)
        // debug may follow in the markdown, we don't care about that

        implicit val s: Scheduler = tinkerSystem.actorSystem.scheduler
        probe.getSideEffects() should be(Nil)
      }
    }

    "be able to indicate satisfactory sleep" in {
      haltoTestBasics(maybeLastAte = None, transcriptionToBroadcast = None) { (tinkerSystem, todaysHaltoNoteRef, halto, probe) =>
        // ignore first write, before hunger message
        //        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler)

        val sleepDurationMinutes = 6.hours.plus(10.minutes)
        halto ! Halto.ReceiveSleepReport(SleepReport(
          // HACKY
          List(SleepEntry(dateOfSleep = "", duration = sleepDurationMinutes, efficiency = 0, endTime = ZonedDateTime.now(), isMainSleep = true, levels = LevelsData(Nil, Map.empty), logId = 0, minutesAfterWakeup = 0, minutesAsleep = 0, minutesAwake = 0, minutesToFallAsleep = 0, startTime = ZonedDateTime.now(), timeInBed = 0))
          , SleepSummary(sleepDurationMinutes.toMinutes.toInt, -1, -1)))
        //      halto ! Halto.ReceiveFrustrationDetected(DistressDetected())

        // make sure the message had time to propagate FIXME may be better way to do this
        Thread.sleep(100)

        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler) should startWith(
          """# State
            |
            |- Hungry: 🤷
            |- Tired: ✅
            |- High CO2: 🤷""".stripMargin)
        // debug may follow in the markdown, we don't care about that

        implicit val s: Scheduler = tinkerSystem.actorSystem.scheduler
        probe.getSideEffects() should be(Nil)
      }
    }

    "produce side-effects, lights flashing and a push notification, when distress is detected" in {
      haltoTestBasics(maybeLastAte = None, transcriptionToBroadcast = None) { (tinkerSystem, todaysHaltoNoteRef, halto, probe) =>
        // ignore first write, before hunger message
        //        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler)

        val notedTranscription = NotedTranscription(TranscriptionCapture(WhisperResult(WhisperResultContent("", Nil), WhisperResultMetadata(LargeModel, "", "", -1)), ZonedDateTime.now()), NoteId(""))
        halto ! Halto.ReceiveFrustrationDetected(DistressDetected(notedTranscription, Nil))

        // make sure the message had time to propagate FIXME may be better way to do this
        Thread.sleep(100)

        todaysHaltoNoteRef.interceptWrite(tinkerSystem.actorSystem.scheduler) should startWith(
          """# State
            |
            |- Hungry: 🤷
            |- Tired: 🤷 ([[Sleep Report]])
            |- High CO2: 🤷""".stripMargin)
        // debug may follow in the markdown, we don't care about that

        implicit val s: Scheduler = tinkerSystem.actorSystem.scheduler
        val sideEffects = probe.getSideEffects()
        sideEffects should have(size(2))
        sideEffects.foreach {
          case NotificationCenterManager.PushNotification(key, message) =>
            key should equal(SearchSpaceKey)
            // FIXME: something with message
          case NotificationCenterManager.HueCommand(command) =>
            command should equal(HueControl.DoALightShow())

          case Chime(_) => ???
        }
      }
    }
  }

  def haltoTestBasics(maybeLastAte: Option[ZonedDateTime], transcriptionToBroadcast: Option[NotedTranscription])(f: (TinkerSystem, VirtualNoteRef, ActorRef[Halto.Event], ActorRef[TinkerProbeActor.Message]) => Unit): TinkerTest = {
    val now = ZonedDateTime.now()
    val todayIso = TimeUtil.localDateToISO8601Date(now.toLocalDate)

    val testKit: SpiritTestKit = new SpiritTestKit()
    val helper: ActorRef[NoteRefWatcherHelper.Message] = testKit.spawn(NoteRefWatcherHelper(), "NoteRefWatcherHelper")

    val lastAteNoteRef = maybeLastAte match {
      case Some(lastAte) =>
        new VirtualNoteRef("last_ate", lastAte.toString)
      case None =>
        new VirtualNoteRef("last_ate")
    }

    val sleepReportNoteRef = new VirtualNoteRef("Sleep Report")
    val todaysHaltoNoteRef = new VirtualNoteRef(s"HALT-O notes ($todayIso)", "", Some(helper))

    val NoteRefs: Map[String, NoteRef] = List(lastAteNoteRef, sleepReportNoteRef, todaysHaltoNoteRef)
      .map(ref => ref.noteId.id -> ref)
      .toMap

    val sleepReportJsonRef = JsonRefForTesting("sleep_report")
    sleepReportJsonRef.set("")
    val haltoJsonRef = JsonRefForTesting(s"halto_notes_$todayIso")
    haltoJsonRef.set("")
    val JsonRefs: Map[String, JsonRef] = List(sleepReportJsonRef, haltoJsonRef)
      .map(ref => ref.filename -> ref)
      .toMap

    new TinkerTest(NoteRefs, JsonRefs, testKit) {
      transcriptionToBroadcast.foreach { notedTranscription =>
        gossiper ! Gossiper.StartTinkering(tinker)
        gossiper ! Gossiper.Receive(notedTranscription)
      }

      val fitbitTestProbe: TestProbe[FitbitActor.Message] = testKit.createTestProbe[FitbitActor.Message]("FitbitTestProbe")
      val halto: ActorRef[Halto.Event] = testKit.spawn(Halto(tinkerSystem.wrap(fitbitTestProbe.ref), NtfyKeys(None, None, Some(SearchSpaceKey))), "Halto")

      f(tinkerSystem, todaysHaltoNoteRef, halto, tinkerTestProbe)

      shutdown() // FIXME: necessary
    }
  }
}
