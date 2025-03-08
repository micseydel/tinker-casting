package me.micseydel.actor.kitties

import akka.actor.typed.ActorRef
import akka.event.slf4j.Logger
import me.micseydel.actor.kitties.kibble.KibbleManagerActor
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.testsupport._
import me.micseydel.vault.NoteId

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.unused

class KibbleManagerActorTestingSpec extends TestTinkerContainer {
  "KibbleManagerActor" must {

    implicit val log: org.slf4j.Logger = Logger("kibbletestlogger")

    "manipulate be able to markdown" in {
      val kibbleMarkdownUpdateTestResult = KibbleManagerActor.addToMarkdown(
        """# Summary
          |
          |- Whatever I freaking imperfectlyl want
          |
          |# Inbox
          |
          |- stuff that I remove from
          |- ...but gets added to
          |
          |# History
          |
          |- stuff that I don't remove from
          |- ...and gets added to
          |""".stripMargin, "- NEW LINE")

      kibbleMarkdownUpdateTestResult shouldEqual
        """# Summary
          |
          |- Whatever I freaking imperfectlyl want
          |
          |# Inbox
          |
          |- stuff that I remove from
          |- ...but gets added to
          |- NEW LINE
          |
          |# History
          |
          |- stuff that I don't remove from
          |- ...and gets added to
          |- NEW LINE
          |""".stripMargin
    }

    "manipulate markdown by receiving messages and updating a NoteRef" in {
      val testKit: SpiritTestKit = new SpiritTestKit()
      val helper: ActorRef[NoteRefWatcherHelper.Message] = testKit.spawn(NoteRefWatcherHelper(), "NoteRefWatcherHelper")

      val NoteName = "Kibble Tinkering 2.0"
      val noteRef: VirtualNoteRef = new VirtualNoteRef(NoteName, helper = Some(helper))
      val NoteRefs = Map(NoteName -> noteRef)

      new TinkerTest(NoteRefs, Map.empty, testKit) {
        gossiper ! Gossiper.StartTinkering(tinker)

        @unused // this contains an internal listener and writes to a NoteRef
        val kibbleManagerActor: ActorRef[KibbleManagerActor.Message] = testKit.spawn(KibbleManagerActor(), "KibbleManagerActor")

        // transcriptionStartedTime:
        val transcriptions: List[(String, String)] = List(
          // 3rd
          ("2024-11-03T07:10:12-08:00[America/Los_Angeles]",
            "I just measured the larger rectangular container as having a mass of 545 grams of Gilmore and the container so not just the dry food.")
          // 4th
          , ("2024-11-04T07:01:46-08:00[America/Los_Angeles]",
            "I just measured 430 grams of remaining kibble in the large rectangular dry food container.")
          // 5th
          , ("2024-11-05T06:17:41-08:00[America/Los_Angeles]",
            "I just measured the large rectangular kibble container as containing 289 grams with the dry food.")
          , ("2024-11-05T07:06:57-08:00[America/Los_Angeles]",
            "I just discarded 24 grams of kibble.")
          , ("2024-11-05T23:47:58-08:00[America/Los_Angeles]",
            "I just refilled the rectangular large kibble to 558 grams.")
          //          , ("2024-11-05T23:49:21-08:00[America/Los_Angeles]",
          //            "I just refilled the small rectangular kibble to 542 grams.")
          // 6th
          , ("2024-11-06T07:18:56-08:00[America/Los_Angeles]",
            "I just discarded kibble, 32 grams of it.")
          , ("2024-11-06T07:19:45-08:00[America/Los_Angeles]",
            "I just measured the large rectangular kibble container as containing 502 grams of dry food plus the container.")
          // 7th
          , ("2024-11-07T07:05:19-08:00[America/Los_Angeles]",
            "I just measured the large rectangular kibble container as having 392 grams of dry food.")
          // 8th
          , ("2024-11-08T07:15:12-08:00[America/Los_Angeles]",
            "I just measured 273 grams of kibble in the large rectangular dry food container.")
          , ("2024-11-08T07:16:01-08:00[America/Los_Angeles]",
            "I just discarded 23 grams worth of kibble.")
          //          , ("2024-11-09T06:48:07-08:00[America/Los_Angeles]",
          //            "I just discarded 17 grams of kibble.")
          //          , ("2024-11-09T07:03:58-08:00[America/Los_Angeles]",
          //            "I just measured a small rectangular kibble container as containing 499 grams.")
        )

        for ((rawStartTime, text) <- transcriptions) {
          val time = ZonedDateTime.parse(rawStartTime)
          val noteId = NoteId(generateFileName(time))
          gossiper ! Gossiper.Receive(TestHelpers.simpleNotedTranscription(text, time, noteId))
        }

        for (_ <- transcriptions.drop(1)) {
          val ignored = noteRef.interceptWrite.length
          //          println(s"ignoring write of $ignored bytes")
        }

        println()

        val noteContents: String = noteRef.interceptWrite

        noteContents shouldEqual """# Summary
                                   |
                                   |- for the human to play with, uninterrupted
                                   |
                                   |# Inbox
                                   |
                                   |- \[7:10:12AM\] I just measured the larger rectangular container as having a mass of 545 grams of Gilmore and the container so not just the dry food. ([[Transcription for mobile_audio_capture_20241103-071012.wav|ref]])
                                   |- \[7:01:46AM\] I just measured 430 grams of remaining kibble in the large rectangular dry food container. ([[Transcription for mobile_audio_capture_20241104-070146.wav|ref]])
                                   |- \[6:17:41AM\] I just measured the large rectangular kibble container as containing 289 grams with the dry food. ([[Transcription for mobile_audio_capture_20241105-061741.wav|ref]])
                                   |- \[7:06:57AM\] I just discarded 24 grams of kibble. ([[Transcription for mobile_audio_capture_20241105-070657.wav|ref]])
                                   |- \[11:47:58PM\] I just refilled the rectangular large kibble to 558 grams. ([[Transcription for mobile_audio_capture_20241105-234758.wav|ref]])
                                   |- \[7:18:56AM\] I just discarded kibble, 32 grams of it. ([[Transcription for mobile_audio_capture_20241106-071856.wav|ref]])
                                   |- \[7:19:45AM\] I just measured the large rectangular kibble container as containing 502 grams of dry food plus the container. ([[Transcription for mobile_audio_capture_20241106-071945.wav|ref]])
                                   |- \[7:05:19AM\] I just measured the large rectangular kibble container as having 392 grams of dry food. ([[Transcription for mobile_audio_capture_20241107-070519.wav|ref]])
                                   |- \[7:15:12AM\] I just measured 273 grams of kibble in the large rectangular dry food container. ([[Transcription for mobile_audio_capture_20241108-071512.wav|ref]])
                                   |- \[7:16:01AM\] I just discarded 23 grams worth of kibble. ([[Transcription for mobile_audio_capture_20241108-071601.wav|ref]])
                                   |
                                   |# History
                                   |
                                   |- \[7:10:12AM\] I just measured the larger rectangular container as having a mass of 545 grams of Gilmore and the container so not just the dry food. ([[Transcription for mobile_audio_capture_20241103-071012.wav|ref]])
                                   |- \[7:01:46AM\] I just measured 430 grams of remaining kibble in the large rectangular dry food container. ([[Transcription for mobile_audio_capture_20241104-070146.wav|ref]])
                                   |- \[6:17:41AM\] I just measured the large rectangular kibble container as containing 289 grams with the dry food. ([[Transcription for mobile_audio_capture_20241105-061741.wav|ref]])
                                   |- \[7:06:57AM\] I just discarded 24 grams of kibble. ([[Transcription for mobile_audio_capture_20241105-070657.wav|ref]])
                                   |- \[11:47:58PM\] I just refilled the rectangular large kibble to 558 grams. ([[Transcription for mobile_audio_capture_20241105-234758.wav|ref]])
                                   |- \[7:18:56AM\] I just discarded kibble, 32 grams of it. ([[Transcription for mobile_audio_capture_20241106-071856.wav|ref]])
                                   |- \[7:19:45AM\] I just measured the large rectangular kibble container as containing 502 grams of dry food plus the container. ([[Transcription for mobile_audio_capture_20241106-071945.wav|ref]])
                                   |- \[7:05:19AM\] I just measured the large rectangular kibble container as having 392 grams of dry food. ([[Transcription for mobile_audio_capture_20241107-070519.wav|ref]])
                                   |- \[7:15:12AM\] I just measured 273 grams of kibble in the large rectangular dry food container. ([[Transcription for mobile_audio_capture_20241108-071512.wav|ref]])
                                   |- \[7:16:01AM\] I just discarded 23 grams worth of kibble. ([[Transcription for mobile_audio_capture_20241108-071601.wav|ref]])
                                   |""".stripMargin

        shutdown() // FIXME: necessary
      }
    }


//    "TESTING" in {
//      val testKit: SpiritTestKit = new SpiritTestKit()
//      val helper: ActorRef[NoteRefWatcherHelper.Message] = testKit.spawn(NoteRefWatcherHelper(), "NoteRefWatcherHelper")
//
//      val noteRef: VirtualNoteRef = new VirtualNoteRef("Kibble Tinkering", helper = Some(helper))
//      val NoteRefs = Map("Kibble Tinkering" -> noteRef)
//      val jsonRef: JsonRef = JsonRefForTesting("kibble_tinkering")
//      val JsonRefs = Map("kibble_tinkering" -> jsonRef)
//
//      new TinkerTest(NoteRefs, JsonRefs, testKit) {
//        gossiper ! Gossiper.StartTinkering(tinker)
//
//        @unused // this contains an internal listener and writes to a NoteRef
//        val kibbleManagerActor: ActorRef[KibbleManagerActor.Message] = testKit.spawn(KibbleManagerActor(), "KibbleManagerActor")
//
//        // transcriptionStartedTime:
//        val transcriptions: List[(String, String)] = List(
//          // 3rd
//            ("2024-11-03T07:10:12-08:00[America/Los_Angeles]",
//              "I just measured the larger rectangular container as having a mass of 545 grams of Gilmore and the container so not just the dry food.")
//          // 4th
//          , ("2024-11-04T07:01:46-08:00[America/Los_Angeles]",
//            "I just measured 430 grams of remaining kibble in the large rectangular dry food container.")
//          // 5th
//          , ("2024-11-05T06:17:41-08:00[America/Los_Angeles]",
//            "I just measured the large rectangular kibble container as containing 289 grams with the dry food.")
//          , ("2024-11-05T07:06:57-08:00[America/Los_Angeles]",
//            "I just discarded 24 grams of kibble.")
//          , ("2024-11-05T23:47:58-08:00[America/Los_Angeles]",
//            "I just refilled the rectangular large kibble to 558 grams.")
////          , ("2024-11-05T23:49:21-08:00[America/Los_Angeles]",
////            "I just refilled the small rectangular kibble to 542 grams.")
//          // 6th
//          , ("2024-11-06T07:18:56-08:00[America/Los_Angeles]",
//            "I just discarded kibble, 32 grams of it.")
//          , ("2024-11-06T07:19:45-08:00[America/Los_Angeles]",
//            "I just measured the large rectangular kibble container as containing 502 grams of dry food plus the container.")
//          // 7th
//          , ("2024-11-07T07:05:19-08:00[America/Los_Angeles]",
//            "I just measured the large rectangular kibble container as having 392 grams of dry food.")
//          // 8th
//          , ("2024-11-08T07:15:12-08:00[America/Los_Angeles]",
//            "I just measured 273 grams of kibble in the large rectangular dry food container.")
//          , ("2024-11-08T07:16:01-08:00[America/Los_Angeles]",
//            "I just discarded 23 grams worth of kibble.")
////          , ("2024-11-09T06:48:07-08:00[America/Los_Angeles]",
////            "I just discarded 17 grams of kibble.")
////          , ("2024-11-09T07:03:58-08:00[America/Los_Angeles]",
////            "I just measured a small rectangular kibble container as containing 499 grams.")
//        )
//
//        for ((rawStartTime, text) <- transcriptions) {
//          val time = ZonedDateTime.parse(rawStartTime)
//          val noteId = NoteId(generateFileName(time))
//          gossiper ! Gossiper.Receive(TestHelpers.simpleNotedTranscription(text, time, noteId))
//        }
//
//        for (_ <- transcriptions.drop(1)) {
//          val ignored = noteRef.interceptWrite.length
////          println(s"ignoring write of $ignored bytes")
//        }
//
//        println()
//
//        val noteContents: String = noteRef.interceptWrite
//
//        // FIXME: hacky
//        val section: String = noteContents
//          .split("\n")
//          .dropWhile(!_.startsWith("# Last 7 days")).slice(2, 7)
//          .mkString("\n")
//        println(section)
//        section shouldEqual("""- \[2024-11-08\] 96
//                              |- \[2024-11-07\] 110
//                              |- \[2024-11-06\] 124
//                              |- \[2024-11-05\] 117
//                              |- \[2024-11-04\] 115""".stripMargin)
//        println("\n---\n")
//
//        println(noteContents)
//
//        shutdown() // FIXME: necessary
//      }
//    }
  }

  private def generateFileName(timestamp: ZonedDateTime): String = {
    // Define a formatter for the desired date pattern
    val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

    // Format the ZonedDateTime and construct the file name
    s"Transcription for mobile_audio_capture_${timestamp.format(formatter)}.wav"
  }
}
