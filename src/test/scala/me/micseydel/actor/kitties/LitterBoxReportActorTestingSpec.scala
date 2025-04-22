package me.micseydel.actor.kitties

import akka.actor.typed.ActorRef
import akka.event.slf4j.Logger
import me.micseydel.model._
import me.micseydel.testsupport._
import me.micseydel.util.TimeUtil
import me.micseydel.vault.NoteId

import java.time.{LocalDate, ZonedDateTime}
import java.time.format.DateTimeFormatter

class LitterBoxReportActorTestingSpec extends TestTinkerContainer {
  val now: ZonedDateTime = ZonedDateTime.now()
  val forDay: LocalDate = now.toLocalDate
  val isoDate: String = TimeUtil.localDateTimeToISO8601Date(forDay)

  "LitterBoxReportActor" must {
    "FIXME" in {
      helper(
        generateEvents(List("17:33:40" -> SiftedContents(1, 1))),
        """# Summary
                                 |
                                 |- Total pee: 1
                                 |- Total poo: 1
                                 |
                                 |# Events
                                 |
                                 |- \[5:33:40PM\] 💦💩 ([[test0|ref]])
                                 |""".stripMargin
      )
    }
  }

  private def generateEvents(list: List[(String, SiftedContents)]): List[LitterBoxReportActor.LitterSiftedObservation] = {
    list.zipWithIndex.map {
      case ((time, contents), index) =>
        LitterBoxReportActor.LitterSiftedObservation(LitterBoxesHelper.LitterSifted(
          LitterSiftedEvent(ZonedDateTime.parse(s"${isoDate}T$time-07:00[America/Los_Angeles]"), BackLitter, contents), NoteId("test" + index)
        ))
    }
  }

  private def helper(events: List[LitterBoxReportActor.LitterSiftedObservation], expected: String): Unit = {
    val testKit: SpiritTestKit = new SpiritTestKit()
    val helper: ActorRef[NoteRefWatcherHelper.Message] = testKit.spawn(NoteRefWatcherHelper(), "NoteRefWatcherHelper")

    val NoteName = s"Litter boxes sifting ($isoDate)"
    val noteRef: VirtualNoteRef = new VirtualNoteRef(NoteName, helper = Some(helper))
    val NoteRefs = Map(NoteName -> noteRef)

    new TinkerTest(NoteRefs, Map.empty, testKit) {
      val litterBoxReportActor: ActorRef[LitterBoxReportActor.Message] = testKit.spawn(LitterBoxReportActor(), "LitterBoxReportActor")

      for (event <- events) {
        litterBoxReportActor ! event
      }

      Thread.sleep(200) // FIXME: better way to do this?

      val noteContents: String = noteRef.interceptWrite

      noteContents shouldEqual expected

      shutdown() // FIXME: necessary
    }
  }
}
