package me.micseydel.actor.kitties

import cats.data.Validated
import cats.data.Validated.Invalid
import me.micseydel.actor.kitties.MarkdownDailyLitterSummaryReportDocumentParser.LineParser

import java.time.LocalDate

object ManualTesting {
  val OriginalMarkdown: String =
    s"""# Summary
       |
       |- Total pee: 6
       |- Total poo: 2
       |- [ ] Audited
       |
       |# Inbox
       |
       |- \\[10:08:39PM\\]  I just lifted a couple of P-clumps from the front letterbox. ([[Transcription for mobile_audio_capture_20260429-220839.wav|ref]])
       |
       |# Events
       |
       |- \\[2:49:13AM\\] 💦💦💩💩 ([[Transcription for mobile_audio_capture_20260429-024913.wav|ref]])
       |    - I just sifted two pee clumps and **one or two poops** from the front litter box.
       |- \\[9:24:01AM\\] 💦💦 ([[Transcription for mobile_audio_capture_20260429-092401.wav|ref]])
       |
       |""".stripMargin

  val Markdown: String =
    s"""# Summary
       |
       |- Total pee: 6
       |- Total poo: 2
       |- [ ] Audited
       |
       |# Inbox
       |
       |- \\[10:08:39PM\\] 11 ([[Transcription for mobile_audio_capture_20260429-220839.wav|ref]])
       |
       |# Events
       |
       |- \\[2:49:13AM\\] 💦💦💩💩 ([[Transcription for mobile_audio_capture_20260429-024913.wav|ref]])
       |    - I just sifted two pee clumps and **one or two poops** from the front litter box.
       |- \\[9:24:01AM\\] 💦💦 ([[Transcription for mobile_audio_capture_20260429-092401.wav|ref]])
       |
       |""".stripMargin

  def main(args: Array[String]): Unit = {
    val forDay = LocalDate.now() // lazy
    new MarkdownDailyLitterSummaryReportDocumentParser(LocalDate.now())(Markdown) match {
      case Invalid(e) => println(e)
      case Validated.Valid(document@DailyLitterDocument(report, inbox)) =>
        val (leftoverInbox, inboxCorrections) = document.inbox.map(LineParser.apply(_, forDay)).partitionMap {
          case MarkdownDailyLitterSummaryReportDocumentParser.ParseSuccessDatapoint(datapoint) =>
            Right(datapoint)

          case MarkdownDailyLitterSummaryReportDocumentParser.ParseFailure(rawLine, reason, comments) =>
            println(s"!! parse failure: $reason ($comments)")
            Left(rawLine)
        }

        val latestDocument = if (inboxCorrections.nonEmpty) {
          println(s"integrating: $inboxCorrections")
          inboxCorrections.foldRight(document) { (correction, documentSoFar) =>
            documentSoFar.append(correction)
          }.copy(inbox = leftoverInbox)
        } else {
          println("no corrections")
          document
        }

        println()
        println(latestDocument.toMarkdown)
    }
  }
}
