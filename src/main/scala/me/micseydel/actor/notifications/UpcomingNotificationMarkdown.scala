package me.micseydel.actor.notifications

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import me.micseydel.NoOp
import me.micseydel.actor.kitties.CatNotificationsManager.LitterNotification
import me.micseydel.actor.notifications.NotificationCenterManager.{Notification, NotificationId}
import me.micseydel.actor.notifications.UpcomingNotificationMarkdown.LineParser.Result
import me.micseydel.dsl.TinkerClock
import me.micseydel.util.{MarkdownUtil, ParseUtil}
import me.micseydel.util.ParseUtil.{LineParseResult, ParseFailure, ParseSuccessDatapoint, getZonedDateTimeFromListLineFront}
import me.micseydel.vault.persistence.{BasicNoteRef, NoteRef}
import me.micseydel.vault.{Note, NoteId}
import org.slf4j.Logger

import java.io.FileNotFoundException
import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object UpcomingNotificationMarkdown {
  def addUpcomingNotification(noteRef: NoteRef, notification: Notification)(tinkerClock: TinkerClock, log: Logger): Try[NoOp.type] = {
    noteRef.readMarkdown().recoverWith {
      case _: FileNotFoundException =>
        Success("")
    }.map { existingMarkdown =>
      val parseResults: List[LineParseResult[Result]] = if (existingMarkdown.isEmpty) {
        Nil
      } else {
        existingMarkdown.split("\n").toList.filter(_.nonEmpty).map(UpcomingNotificationMarkdown.LineParser.apply(_,
          tinkerClock.now().toLocalDate // assuming anything there is from today FIXME
        ))
      }

      parseResults.partitionMap {
        case ParseSuccessDatapoint(datapoint) => Right(datapoint.toNotification)
        case l@ParseFailure(_, _, _) => Left(l)
      }
    }.map {
      case (failures, upcomingNotifications) =>
        if (failures.nonEmpty) {
          val details = failures.map {
            case ParseFailure(rawLine, reason, _) =>
              s"- Failure $reason for line: $rawLine"
          }.mkString("\n")
          log.warn(s"Had ${failures.size} parsing failures, falling back to simple file append; details:\n$details")

          noteRef.appendLine(Notification.toMarkdownListLine(notification, checkboxes = false))
        } else {
          log.info(s"Adding notification ${notification.notificationId} for time ${notification.time}")
          log.info(s"!! Adding $notification to existing $upcomingNotifications")
          val updatedUpcomingNotifications: List[Notification] =
            (notification :: upcomingNotifications.filterNot(_.notificationId == notification.notificationId))
              .sortBy(_.time)
          noteRef.setMarkdown(updatedUpcomingNotifications.map(Notification.toMarkdownListLine(_, checkboxes = false)).mkString("\n"))
        }

        NoOp
    }
  }

  def removeUpcomingNotification(noteRef: NoteRef, notificationId: String): Try[NoOp.type] = {
    noteRef
      .updateMarkdown(MarkdownUtil.removeLinesEndingWithBlockId(notificationId, _))
      .map(_ => NoOp)
  }

  // abstraction

  object LineParser {
    case class Result(time: ZonedDateTime, contents: String, wikiLink: Option[WikiLink], notificationId: NotificationId) {
      def toMarkdown: String = {
        // \[7:00pm\] litter will need sifting at ([[nonexistent sample note|scheduled at 6:30pm]]) ^ExampleLitter
        val maybeWikiLink = wikiLink.map(wl => s"($wl) ")
        MarkdownUtil.listLineWithTimestamp(
          time,
          s"$contents ^$notificationId" // s"$contents ${maybeWikiLink}^$notificationId"
        )
      }

      def toNotification: Notification = {
        Notification(time, s"$contents ($maybeWikiLink)", None, notificationId, Nil)
      }

      private def maybeWikiLink: Option[String] = wikiLink.map(wl => s"($wl) ")
    }

    def apply(line: String, day: LocalDate): LineParseResult[Result] = {
      // e.g.
      // \[7:00pm\] litter will need sifting at ([[nonexistent sample note|scheduled at 6:30pm]]) ^ExampleLitter
      // List(-, \[07:29:22PM\], litter, will, need, sifting, at, ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]), ^ExamplelLitter)
      val validated = getZonedDateTimeFromListLineFront(line.split(' ').toList, day).andThen {
        case (entryTime, theRest) =>
          getContentsNoteIdAndNotificationId(theRest).map {
            case (contents,
            //            wikilink,
            notificationId) =>
              Result(entryTime, contents,
                None, // Some(wikilink),
                notificationId)
          }
      }

      validated match {
        case Validated.Valid(datapoint) =>
          ParseSuccessDatapoint(datapoint)
        case Validated.Invalid(reasons) =>
          ParseFailure(line, reasons, Nil)
      }
    }

    private def getContentsNoteIdAndNotificationId(tokens: List[String]): ValidatedNel[String, (String, NotificationId)] = {
      // List(litter, will, need, sifting, at, ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]))
      getNotificationIdFromEnd(tokens).andThen {
        case (theRest, notificationId: NotificationId) =>
          betweenTimeAndNotificationId(theRest).map {
            //case (contents, wikilink) =>
            contents =>
              (contents,
                //                wikilink,
                notificationId)
          }
      }
    }

    private def getNotificationIdFromEnd(tokens: List[String]): ValidatedNel[String, (List[String], NotificationId)] = {
      // List(litter, will, need, sifting, at, ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]), ^ExampleLitter)
      tokens.reverse match {
        case Nil =>
          Validated.Invalid(NonEmptyList.of("Expected an Obsidian block id at the end of the line"))
        case notificationIdStr :: theRest if notificationIdStr.startsWith("^") =>
          Validated.Valid((theRest.reverse, NotificationId(notificationIdStr.drop(1))))
        case otherStart :: _ =>
          Validated.Invalid(NonEmptyList.of(s"Expected token at end of line `$otherStart` to start with a carrot (^) but it did not"))
      }
    }

    private def betweenTimeAndNotificationId(tokens: List[String]): ValidatedNel[String,
      String // (String, WikiLink)
    ] = {
      // List(litter, will, need, sifting, at, ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]))
      //      getParenthesizedWikilinkFromEnd(tokens).map {
      //        case (remainingTokens, wikilink) =>
      //          (remainingTokens.mkString(" "), wikilink)
      //      }
      Validated.Valid(tokens.mkString(" "))
    }

    case class WikiLink(noteId: NoteId, maybeAlias: Option[String]) {
      override def toString: String = {
        maybeAlias match {
          case None => s"[[$noteId]]"
          case Some(alias) => s"[[$noteId|$alias]]"
        }
      }
    }

//    private def getParenthesizedWikilinkFromEnd(tokens: List[String]): ValidatedNel[String, (List[String], WikiLink)] = {
//      // List(litter, will, need, sifting, at, ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]))
//
//      def splitByBar(s: String): ValidatedNel[String, (String, Option[String])] = {
//        s.split("\\|").toList match {
//          case Nil =>
//            Validated.Valid(s, None)
//
//          case head :: Nil =>
//            Validated.Valid(head, None)
//
//          case left :: right :: Nil =>
//            Validated.Valid(left, Some(right))
//
//          case bigger =>
//            val count = bigger.count(_ == "|")
//            Validated.Invalid(NonEmptyList.of(s"Expected at most one bar (|) but found $count"))
//        }
//      }
//
//      @tailrec
//      def splitTokensByBar(tokensToSplit: List[String], accumulator: List[String]): ValidatedNel[String, (List[String], List[String])] = {
//        tokensToSplit match {
//          case Nil =>
//            Validated.Valid((accumulator.reverse, Nil))
//
//          case head :: tail if head.contains("|") =>
//            if (tail.exists(_.contains("|"))) {
//              Validated.Invalid(NonEmptyList.of(s"Expected at most one bar (|) but found at least two: one in $head and somewhere in $tail"))
//            } else {
//              splitByBar(head).map {
//                case (left, Some(right)) =>
//                  ((left :: accumulator).reverse, right :: tail)
//                case (str, None) =>
//                  ((str :: accumulator).reverse, tail)
//              }
//            }
//
//          case head :: tail =>
//            splitTokensByBar(tail, head :: accumulator)
//        }
//      }
//
//      def getWikilinkFromReversedTokens(wikiLinkTokens: List[String]): ValidatedNel[String, WikiLink] = {
//        def wikiLink(noteIdTokens: List[String], aliasTokens: List[String] = Nil): WikiLink = {
//          WikiLink(NoteId(noteIdTokens.reverse.mkString(" ")), Some(aliasTokens.reverse.mkString(" ")))
//        }
//
//        wikiLinkTokens.indexWhere(_.contains("|")) match {
//          case -1 =>
//            Validated.Valid(wikiLink(wikiLinkTokens))
//          case index =>
//            splitTokensByBar(wikiLinkTokens.slice(0, index), Nil).map {
//              case (noteIdTokens, Nil) =>
//                wikiLink(noteIdTokens)
//              case (noteIdTokens, aliasTokens) =>
//                wikiLink(noteIdTokens, aliasTokens)
//            }
//        }
//      }
//
//      @tailrec
//      def helper(remainingTokens: List[String], accumulator: List[String]): ValidatedNel[String, (List[String], WikiLink)] = {
//        // List(..., ([[Transcription, for, mobile_audio_capture_20240218-192922.wav|ref]]))
//        // List(mobile_audio_capture_20240218-192922.wav|ref]]), for, ([[Transcription, ...)
//        remainingTokens match {
//          case head :: tail if head.startsWith("([[") =>
//            getWikilinkFromReversedTokens(head.drop(3) :: accumulator).map { wikiLink =>
//              (tail, wikiLink)
//            }
//
//          case head :: tail =>
//            helper(tail, head :: accumulator)
//
//          case Nil =>
//            Validated.Invalid(NonEmptyList.of("Expected tokens for a note id, found no tokens"))
//        }
//      }
//
//      val reversed = tokens.reverse
//      reversed.headOption match {
//        case Some(endingToken) if endingToken.endsWith("]])") =>
//          helper(endingToken.dropRight(3) :: reversed.tail, Nil)
//        case other =>
//          val found = other.getOrElse("(nothing)")
//          Validated.Invalid(NonEmptyList.of(s"Expected token ending with `]])` but found: $found"))
//      }
//
//    }
  }

  def main(args: Array[String]): Unit = {
    //    val toParse = "- \\[10:02:03AM\\] FrontLitter needs sifting ^FrontLitter"
    //    val parsed: LineParseResult[LineParser.Result] = LineParser(toParse, LocalDate.now())
    //    println(parsed)

    def removeUpcomingNotification(markdown: String, notificationId: String): String = {
//      noteRef.updateMarkdown { markdown =>
        val lines = markdown.split("\\n").filter(_.trim.nonEmpty)
        lines
          .filterNot(_.endsWith(s" ^$notificationId"))
          .mkString("\n")
//      }.map(_ => NoOp)
    }

    val t = s"""- \\[2:20:33AM\\] BackLitter needs sifting ([[Transcription for mobile_audio_capture_20250206-020633.wav|ref]]) ^BackLitter""".stripMargin

    println(removeUpcomingNotification(t, "BackLitter"))

//    println(Notification.toMarkdownListLine(LitterNotification(
//      ZonedDateTime.now(),
//      "FrontLitter needs sifting",
//      NoteId("test"),
//      NotificationId("FrontLitter")
//    ), checkboxes = false))
  }
}
