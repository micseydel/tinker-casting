package me.micseydel.actor.kitties

import akka.actor.typed.ActorRef
import akka.event.slf4j.Logger
import me.micseydel.actor.kitties.kibble.KibbleManagerActor
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.model.{Entity, Intent, KnownIntent, NotedTranscription, RasaResult}
import me.micseydel.testsupport._
import me.micseydel.vault.NoteId

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.unused

class LitterTrackingDashboardActorTestingSpec extends TestTinkerContainer {
  "KibbleManagerActor" must {

    implicit val log: org.slf4j.Logger = Logger("litterdashboardtestlogger")

    "FIXME" in {
      val testKit: SpiritTestKit = new SpiritTestKit()
      val helper: ActorRef[NoteRefWatcherHelper.Message] = testKit.spawn(NoteRefWatcherHelper(), "NoteRefWatcherHelper")

      val NoteName = LitterTrackingDashboardActor.NoteName
      val noteRef: VirtualNoteRef = new VirtualNoteRef(NoteName, helper = Some(helper))
      val NoteRefs = Map(NoteName -> noteRef)

      new TinkerTest(NoteRefs, Map.empty, testKit) {
        val litterTrackingDashboardActor: ActorRef[LitterTrackingDashboardActor.Message] = testKit.spawn(LitterTrackingDashboardActor(), "LitterTrackingDashboardActor")

        val text = "test"
        val notedTranscription: NotedTranscription = {
          val rawStartTime = "2024-11-09T07:03:58-08:00[America/Los_Angeles]"
          val time = ZonedDateTime.parse(rawStartTime)
          val noteId = NoteId(generateFileName(time))
          TestHelpers.simpleNotedTranscription(text, time, noteId)
        }

        val rasaResult: RasaResult = {
          val entities = List(Entity(0.89, None, 0, "urine", "extractor", None, 0, "two"))
          val intent = Intent(0.9, KnownIntent.observe_sifted_contents.IntentName)
          val intentRanking = Nil
          val textTokens = Nil
          RasaResult(entities, intent, intentRanking, text, textTokens)
        }

        litterTrackingDashboardActor ! LitterTrackingDashboardActor.PartialMatch(notedTranscription, rasaResult)

        Thread.sleep(200) // FIXME: better way to do this?

        val noteContents: String = noteRef.interceptWrite

        noteContents shouldEqual """- \[7:03:58AM\] (entity extraction failed) test ([[Transcription for mobile_audio_capture_20241109-070358.wav|ref]])
                                   |        - urine two 0.89 (None, None)
                                   |""".stripMargin

        shutdown() // FIXME: necessary
      }
    }
  }

  private def generateFileName(timestamp: ZonedDateTime): String = {
    // Define a formatter for the desired date pattern
    val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

    // Format the ZonedDateTime and construct the file name
    s"Transcription for mobile_audio_capture_${timestamp.format(formatter)}.wav"
  }
}
