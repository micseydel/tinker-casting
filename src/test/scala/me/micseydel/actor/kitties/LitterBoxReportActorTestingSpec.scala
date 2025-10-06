//package me.micseydel.actor.kitties
//
//import akka.actor.typed.ActorRef
//import akka.event.slf4j.Logger
//import me.micseydel.actor.kitties.LitterBoxReportActor.AddToInbox
//import me.micseydel.model._
//import me.micseydel.testsupport._
//import me.micseydel.util.TimeUtil
//import me.micseydel.vault.NoteId
//
//import java.time.{LocalDate, ZonedDateTime}
//import java.time.format.DateTimeFormatter
//
//class LitterBoxReportActorTestingSpec extends TestTinkerContainer {
//  val now: ZonedDateTime = ZonedDateTime.now() // parse("2025-04-21T18:49:25.398941-07:00[America/Los_Angeles]")
//  val forDay: LocalDate = now.toLocalDate
//  val isoDate: String = TimeUtil.localDateTimeToISO8601Date(forDay)
//  val isoDateDigits: String = isoDate.filter(_.isDigit)
//
//  val NoteName = s"Litter boxes sifting ($isoDate)"
//  val YesterdaysNoteName = s"Litter boxes sifting (${TimeUtil.localDateTimeToISO8601Date(forDay.minusDays(1))})"
//  val TheDayBeforeYesterdaysNoteName = s"Litter boxes sifting (${TimeUtil.localDateTimeToISO8601Date(forDay.minusDays(2))})"
//
//  "LitterBoxReportActor" must {
//    "FIXME" in {
//      helper(
//        generateEvents(List("17:33:40" -> SiftedContents(1, 1))),
//        s"""# Summary
//          |
//          |- Total pee: 1
//          |- Total poo: 1
//          |
//          |# Events
//          |
//          |- \\[5:33:40PM\\] 💦💩 ([[Transcription for mobile_audio_capture_$isoDateDigits-173340.wav|ref]])
//          |""".stripMargin
//      )
//    }
//
//    "FIXME2" in {
//      helper(
//        generateEvents(List(
//          "17:33:41" -> SiftedContents(0, 1),
//          "19:52:16" -> SiftedContents(1, 0),
//          "20:40:52" -> SiftedContents(1, 1)
//        )),
//        s"""# Summary
//          |
//          |- Total pee: 2
//          |- Total poo: 2
//          |
//          |# Events
//          |
//          |- \\[5:33:41PM\\] 💩 ([[Transcription for mobile_audio_capture_$isoDateDigits-173341.wav|ref]])
//          |- \\[7:52:16PM\\] 💦 ([[Transcription for mobile_audio_capture_$isoDateDigits-195216.wav|ref]])
//          |- \\[8:40:52PM\\] 💦💩 ([[Transcription for mobile_audio_capture_$isoDateDigits-204052.wav|ref]])
//          |""".stripMargin
//      )
//    }
//
//    "FIXME3" in {
//      helper(
//        AddToInbox("test", ZonedDateTime.parse(s"${isoDate}T11:33:41-07:00[America/Los_Angeles]")) :: generateEvents(List(
//          "17:33:41" -> SiftedContents(0, 1),
//          "19:52:16" -> SiftedContents(1, 0),
//          "20:40:52" -> SiftedContents(1, 1)
//        )) ::: List(
//          AddToInbox("test2", ZonedDateTime.parse(s"${isoDate}T21:39:21-07:00[America/Los_Angeles]")),
//          AddToInbox("test3", ZonedDateTime.parse(s"${isoDate}T22:39:21-07:00[America/Los_Angeles]")),
//          AddToInbox("test4", ZonedDateTime.parse(s"${isoDate}T23:39:21-07:00[America/Los_Angeles]"))
//        ),
//
//        s"""# Summary
//          |
//          |- Total pee: 2
//          |- Total poo: 2
//          |
//          |# Inbox
//          |
//          |- test
//          |- test2
//          |- test3
//          |- test4
//          |
//          |# Events
//          |
//          |- \\[5:33:41PM\\] 💩 ([[Transcription for mobile_audio_capture_$isoDateDigits-173341.wav|ref]])
//          |- \\[7:52:16PM\\] 💦 ([[Transcription for mobile_audio_capture_$isoDateDigits-195216.wav|ref]])
//          |- \\[8:40:52PM\\] 💦💩 ([[Transcription for mobile_audio_capture_$isoDateDigits-204052.wav|ref]])
//          |""".stripMargin
//      )
//    }
//  }
//
//  private def generateEvents(list: List[(String, SiftedContents)]): List[LitterBoxReportActor.EventCapture] = {
//    list.map {
//      case (time, contents) =>
//        val timestamp = ZonedDateTime.parse(s"${isoDate}T$time-07:00[America/Los_Angeles]")
//        LitterBoxReportActor.LitterSiftedObservation(LitterBoxesHelper.LitterSifted(
//          LitterSiftedEvent(timestamp, BackLitter, contents), NoteId(generateFileName(timestamp))
//        ))
//    }
//  }
//
//  private def helper(events: List[LitterBoxReportActor.EventCapture], expected: String): Unit = {
//    val testKit: SpiritTestKit = new SpiritTestKit()
//    val helper: ActorRef[NoteRefWatcherHelper.Message] = testKit.spawn(NoteRefWatcherHelper(), "NoteRefWatcherHelper")
//
//    val noteRef: VirtualNoteRef = new VirtualNoteRef(NoteName, helper = Some(helper))
//    val NoteRefs = Map(NoteName -> noteRef, YesterdaysNoteName -> new VirtualNoteRef(YesterdaysNoteName), TheDayBeforeYesterdaysNoteName -> new VirtualNoteRef(TheDayBeforeYesterdaysNoteName))
//
//    new TinkerTest(NoteRefs, Map.empty, testKit) {
//      val litterBoxReportActor: ActorRef[LitterBoxReportActor.Message] = testKit.spawn(LitterBoxReportActor(), "LitterBoxReportActor")
//
//      for (event <- events) {
//        litterBoxReportActor ! event
//      }
//
//      Thread.sleep(200) // FIXME: better way to do this?
//
//      val noteContents: String = noteRef.interceptWrite
//
//      noteContents shouldEqual expected
//
//      shutdown() // FIXME: necessary
//    }
//  }
//
//  private def generateFileName(timestamp: ZonedDateTime): String = {
//    // Define a formatter for the desired date pattern
//    val formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")
//
//    // Format the ZonedDateTime and construct the file name
//    s"Transcription for mobile_audio_capture_${timestamp.format(formatter)}.wav"
//  }
//}
