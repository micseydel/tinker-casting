package me.micseydel.actor.kitties

import akka.event.slf4j.Logger
import me.micseydel.TestTinkerContainer
import me.micseydel.actor.kitties.kibble.KibbleManagerActor.{KibbleDiscarded, KibbleRefill, RemainingKibbleMeasure}
import me.micseydel.actor.kitties.kibble.KibbleModel.{KibbleContainer, RectangularL, RectangularS}
import me.micseydel.actor.kitties.kibble.{KibbleManagerActor, KibbleMarkdownGenerator}
import me.micseydel.vault.NoteId

import java.time.ZonedDateTime

class KibbleMarkdownTestingSpec extends TestTinkerContainer {
  "KibbleMarkdownGenerator" must {
    "SIMPLE" in {
      implicit val log: org.slf4j.Logger = Logger("testlogger")

      val events: List[KibbleManagerActor.Event] = List(
        measure(545, "2024-11-03T07:10:12"),
        measure(430, "2024-11-04T07:01:46"),
        measure(289, "2024-11-05T06:17:41"),
        discard(24, "2024-11-05T07:06:57")
      )

      val markdown = KibbleMarkdownGenerator(events)
      val section = markdown
        .split("\n")
        .dropWhile(!_.startsWith("# Last 7 days")).slice(2, 5)
        .mkString("\n")

      println(markdown)

      section shouldEqual("""- \[2024-11-04\] 117
                            |- \[2024-11-03\] 115
                            |""".stripMargin)
    }

    "TESTING" in {
      implicit val log: org.slf4j.Logger = Logger("testlogger")

      val events: List[KibbleManagerActor.Event] = List(
        measure(545, "2024-11-03T07:10:12"),
        measure(430, "2024-11-04T07:01:46"),
        measure(289, "2024-11-05T06:17:41"),
        discard(24, "2024-11-05T07:06:57"),
        refill(558, "2024-11-05T23:47:58"),
        refill(542, "2024-11-05T23:49:21", RectangularS),
        discard(32, "2024-11-06T07:18:56"),
        measure(502, "2024-11-06T07:19:45"),
        measure(392, "2024-11-07T07:05:19"),
        measure(273, "2024-11-08T07:15:12"),
        discard(23, "2024-11-08T07:16:01"),
        discard(17, "2024-11-09T06:48:07"),
        measure(499, "2024-11-09T07:03:58", RectangularS),
        measure(352, "2024-11-10T07:07:49", RectangularS),
        discard(24, "2024-11-11T06:20:38"),
        measure(251, "2024-11-11T06:20:59", RectangularS)
      )

      println(KibbleMarkdownGenerator(events))
    }

    "2024-11-28" in {
      implicit val log: org.slf4j.Logger = Logger("testlogger")

      val events: List[KibbleManagerActor.Event] = List(
        measure(545, "2024-11-03T07:10:12"),
        measure(430, "2024-11-04T07:01:46"),
        measure(289, "2024-11-05T06:17:41"),
        discard(24, "2024-11-05T07:06:57"),
        refill(558, "2024-11-05T23:47:58"),
        refill(542, "2024-11-05T23:49:21", RectangularS),
        discard(32, "2024-11-06T07:18:56"),
        measure(502, "2024-11-06T07:19:45"),
        measure(392, "2024-11-07T07:05:19"),
        measure(273, "2024-11-08T07:15:12"),
        discard(23, "2024-11-08T07:16:01"),
        discard(17, "2024-11-09T06:48:07"),
        measure(499, "2024-11-09T07:03:58", RectangularS),
        measure(352, "2024-11-10T07:07:49", RectangularS),
        discard(24, "2024-11-11T06:20:38"),
        measure(251, "2024-11-11T06:20:59", RectangularS)
      )

      println(KibbleMarkdownGenerator(events))
    }
  }

  // helpers

  private def measure(mass: Int, time: String, container: KibbleContainer = RectangularL): RemainingKibbleMeasure = {
    RemainingKibbleMeasure(container, mass, ZonedDateTime.parse(time + ZonedDateTimeSuffixOffset), NoteId(time))
  }

  private def discard(mass: Int, time: String): KibbleDiscarded = {
    KibbleDiscarded(mass, ZonedDateTime.parse(time + ZonedDateTimeSuffixOffset), NoteId(time))
  }

  private def refill(mass: Int, time: String, container: KibbleContainer = RectangularL): KibbleRefill = {
    KibbleRefill(container, mass, ZonedDateTime.parse(time + ZonedDateTimeSuffixOffset), NoteId(time))
  }

  private val ZonedDateTimeSuffixOffset = "-08:00[America/Los_Angeles]"
}
