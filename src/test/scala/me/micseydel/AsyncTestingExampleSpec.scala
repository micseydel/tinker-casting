package me.micseydel

import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.ActorRef
import me.micseydel.actor.kitties.CatNotificationsManager
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.actor.notifications.{NotificationCenterManager, UpcomingNotificationsManager}
import me.micseydel.model.BackLitter
import me.micseydel.testsupport.{JsonRefForTesting, SpiritTestKit, TestHelpers, TestTinkerContainer, TimeOrchestratorForTesting, TinkerTest, VirtualNoteRef}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.{JsonRef, NoteRef}

import java.time.ZonedDateTime
import scala.util.{Failure, Success}

class AsyncTestingExampleSpec extends TestTinkerContainer {
//  FIXME actorNotesFolderWatcherActor needs an impl for this
//  "UpcomingNotificationsManager" must {
//    "add notification to note via NoteRef upon receiving UpcomingNotification" in {
//      val noteRef = new VirtualNoteRef("Upcoming Notifications")
//      val NoteRefs: Map[String, NoteRef] = Map("Upcoming Notifications" -> noteRef)
//
//      val JsonRefs: Map[String, JsonRef] = Map("upcoming_notifications_queued" -> JsonRefForTesting("upcoming_notifications_queued"))
//      new TinkerTest(NoteRefs, JsonRefs, new SpiritTestKit()) {
//
//        val notificationCenterManagerProbe: TestProbe[NotificationCenterManager.Message] = testKit.createTestProbe[NotificationCenterManager.Message]("NotificationCenterManager")
//
//        val upcomingNotificationsManager: ActorRef[UpcomingNotificationsManager.Message] = testKit.spawn(UpcomingNotificationsManager(
//          tinkerSystem.wrap(notificationCenterManagerProbe.ref)
//        ), "UpcomingNotificationsManager")
//
//        val now: ZonedDateTime = ZonedDateTime.parse("2024-09-23T17:42:56.070373-07:00[America/Los_Angeles]")
//        val notification: Notification = Notification(
//          now,
//          "FrontLitter needs sifting",
//          None,
//          NotificationId("FrontLitter"),
//          Nil
//        )
//        upcomingNotificationsManager ! UpcomingNotificationsManager.UpcomingNotification(notification)
//
//        Thread.sleep(3000) // FIXME HACK HACK
//
//        noteRef.readMarkdown() match {
//          case Failure(exception) => throw exception
//          case Success(markdown) =>
//            if (!markdown.contains("- \\[5:42:56PM\\] FrontLitter needs sifting ^FrontLitter")) {
//              throw new AssertionError(s"non-match for markdown:\n$markdown")
//            }
//        }
//
//        //      timeOrchestrator ! TimeOrchestratorForTesting.Go()
//        TestHelpers.log("Double-checking no messages buffered in time")
//        //      notificationCenterManagerProbe.expectNoMessage(3.seconds)
//        timeOrchestrator ! TimeOrchestratorForTesting.Go()
//        Thread.sleep(2000) // FIXME HACK HACK
//        timeOrchestrator ! TimeOrchestratorForTesting.Go()
//        Thread.sleep(2000) // FIXME HACK HACK
//
//
//        noteRef.readMarkdown() match {
//          case Failure(exception) => throw exception
//          case Success(markdown) =>
//            if (markdown.trim.nonEmpty) {
//              throw new AssertionError(s"${ZonedDateTime.now()} expected markdown to be cleared:\n$markdown")
//            }
//        }
//
//        shutdown() // FIXME: necessary
//      }
//    }
//  }

  "CatNotificationsManager" must {
    "t" in new TinkerTest(Map.empty, Map.empty, new SpiritTestKit()) {

//      val notificationCenterManagerProbe: TestProbe[NotificationCenterManager.Message] = testKit.createTestProbe[NotificationCenterManager.Message]("NotificationCenterManager")

      val catNotificationsManager: ActorRef[CatNotificationsManager.Message] =
        testKit.spawn(CatNotificationsManager(), "CatNotificationsManager")

      catNotificationsManager ! CatNotificationsManager.LitterUsed(
        ZonedDateTime.now(),
        BackLitter,
        None,
        NoteId("test")
      )

//      val notification: Notification = Notification(
//        ZonedDateTime.now(),
//        "FrontLitter needs sifting",
//        None,
//        NotificationId("FrontLitter"),
//        Nil
//      )
//      upcomingNotificationsManager ! UpcomingNotificationsManager.UpcomingNotification(notification)
//
//      TestHelpers.log("Double-checking no messages buffered in time")
//      notificationCenterManagerProbe.expectNoMessage(3.seconds)
//      timeOrchestrator ! TimeOrchestratorForTesting.Go()
//
//      notificationCenterManagerProbe.expectMessage(NotificationCenterManager.NewNotification(notification))


      shutdown() // FIXME: necessary
    }
  }
}

//
////private
//object AsyncTestingExampleSpec {
//  val NoteRefs: Map[String, NoteRef] = Map("Upcoming Notifications" -> new VirtualNoteRef("Upcoming Notifications"))
//  val JsonRefs: Map[String, JsonRef] = Map("upcoming_notifications_queued" -> JsonRefForTesting("upcoming_notifications_queued"))
//}


