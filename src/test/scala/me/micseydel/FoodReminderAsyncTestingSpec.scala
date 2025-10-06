//package me.micseydel
//
//import akka.actor.testkit.typed.scaladsl.TestProbe
//import akka.actor.typed.{ActorRef, Scheduler}
//import akka.actor.typed.scaladsl.AskPattern.Askable
//import akka.util.Timeout
//import me.micseydel.actor.FoodReminderActor
//import me.micseydel.actor.FoodReminderActor.RichTime
//import me.micseydel.actor.HungerTracker.HungerState
//import me.micseydel.actor.notifications.NotificationCenterManager
//import me.micseydel.actor.notifications.NotificationCenterManager.SideEffect
//import me.micseydel.dsl.SpiritRef
//import me.micseydel.testsupport.{NoteRefWatcherHelper, SpiritTestKit, TestTinkerContainer, TinkerProbeActor, TinkerTest, VirtualNoteRef}
//
//import java.time.{LocalDate, LocalTime, ZoneId, ZonedDateTime}
//import java.util.concurrent.TimeUnit
//import scala.concurrent.Await
//import scala.concurrent.duration.{DurationInt, FiniteDuration}
//import scala.util.{Failure, Success}
//
//class FoodReminderAsyncTestingSpec extends TestTinkerContainer {
//  val NtfyKey = "HungryNtfyKey"
//
//  "FoodReminderAsyncTestingSpec" must {
//    "TESTING" in {
//      val initialLastAte = ZonedDateTime.now().minusDays(2)
//      test(initialLastAte) { (setTime, setCaloriesConsumed, readNote, getSideEffects, hungerTrackerProbe) =>
//        val today = LocalDate.now()
//
//        setCaloriesConsumed(ZonedDateTime.of(today, LocalTime.of(7, 0), ZoneId.systemDefault()))
//        println(readNote())
//
//        println(hungerTrackerProbe.receiveMessage())
//
//        setTime(ZonedDateTime.of(today, LocalTime.of(12, 0), ZoneId.systemDefault()))
//
//        Thread.sleep(250)
//
//        println(hungerTrackerProbe.receiveMessage(FiniteDuration(1, TimeUnit.SECONDS)))
//
//        println(getSideEffects())
//      }
//    }
//  }
//
//  private def test(initialLastAte: ZonedDateTime)
//                  (f: (ZonedDateTime => Unit, ZonedDateTime => Unit, () => String, () => List[SideEffect], TestProbe[HungerState]) => Unit): Unit = {
//    val testKit: SpiritTestKit = new SpiritTestKit()
//    val helper: ActorRef[NoteRefWatcherHelper.Message] = testKit.spawn(NoteRefWatcherHelper(), "NoteRefWatcherHelper")
//
//    val noteRef: VirtualNoteRef = new VirtualNoteRef("last_ate", initialLastAte.toString, helper = Some(helper))
//    val NoteRefs = Map("last_ate" -> noteRef)
//
//    new TinkerTest(NoteRefs, Map.empty, testKit) {
//      val hungerTrackerProbe: TestProbe[HungerState] = testKit.createTestProbe[HungerState]("HungerTrackerProbe")
//
//      val actorUnderTest: SpiritRef[FoodReminderActor.Message] = testKit.cast(
//        FoodReminderActor(tinkerSystem.wrap(hungerTrackerProbe.ref), Some(NtfyKey)),
//        "FoodReminderActor"
//      )
//
//      def setTime(time: ZonedDateTime): Unit = {
//        tinkerClockForTest.set(time)
//        actorUnderTest !!!! FoodReminderActor.TimeHasPassed
//      }
//
//      def setCaloriesConsumed(time: ZonedDateTime): Unit = {
//        actorUnderTest !!!! FoodReminderActor.CaloriesConsumed(time)
//      }
//
//      def readNote(): String = {
//        noteRef.interceptWrite
//      }
//
//      f(setTime, setCaloriesConsumed, readNote, tinkerTestProbe.getSideEffects, hungerTrackerProbe)
//
//      shutdown() // FIXME: necessary
//    }
//  }
//
//  "RichTime.isDaylight" must {
//    "respect (hour >= 9 && hour < 22)" in {
//      isDaylight("2024-11-20T08:38:47") shouldBe false
//      isDaylight("2024-11-20T09:38:47") shouldBe true
//      isDaylight("2024-11-20T20:38:47") shouldBe true
//      isDaylight("2024-11-20T23:38:47") shouldBe false
//    }
//  }
//
//  private def isDaylight(shortString: String): Boolean = {
//    ZonedDateTime.parse(shortString + "-08:00[America/Los_Angeles]").isDaylight
//  }
//}
//
//private object FoodReminderHelper {
//  implicit class RichTinkerTestProbe(val tinkerTestProbe: ActorRef[TinkerProbeActor.Message]) extends AnyVal {
//    def getSideEffects()(implicit scheduler: Scheduler): List[NotificationCenterManager.SideEffect] = {
//      val duration: FiniteDuration = 3.seconds
//      implicit val to2: Timeout = Timeout(duration)
//      Await.ready(tinkerTestProbe.ask(TinkerProbeActor.GetSideEffects), duration).value match {
//        case None => throw new RuntimeException()
//        case Some(value) =>
//          value match {
//            case Failure(exception) => throw exception
//            case Success(sideEffects) => sideEffects
//          }
//      }
//    }
//  }
//}
