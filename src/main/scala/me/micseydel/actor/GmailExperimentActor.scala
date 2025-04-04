package me.micseydel.actor

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Tinker, TinkerColor}

import java.time.ZonedDateTime

object GmailExperimentActor {
  sealed trait Message

  private case class ReceiveEmail(emails: Seq[GmailActor.Email]) extends Message

  def apply(gmailConfig: GmailConfig)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Gmail API Integration Testing", TinkerColor.random(), "📮") { (context, noteRef) =>

    val gmailFetcher = context.spawn(GmailActor(gmailConfig), "GmailActor")

    gmailFetcher ! GmailActor.Subscribe(context.messageAdapter(ReceiveEmail).underlying)
    context.actorContext.log.debug("Subscribed to GmailActor")

    Tinker.receiveMessage {
      case ReceiveEmail(emails) =>
        val formattedEmails = emails.map {
          case GmailActor.Email(sender, subject, _, sentAt, _) =>
            s"- \\[$sentAt] \\<$sender> **$subject**"
        }.mkString("", "\n", "\n")

        val formattedHeaders = emails.map {
          case GmailActor.Email(_, _, body, sentAt, headers) =>
            val formattedHeaders = headers.map {
              case (key, List(justOne)) =>
                if (justOne.length > 200) {
                  s"- $key\n    - `$justOne`"
                } else {
                  s"- $key: `$justOne`"
                }
              case (key, list) =>
                (s"- $key" :: list.map(s => s"    - `$s`")).mkString("\n")
            }.mkString("\n")

            s"""## $sentAt
               |
               |$formattedHeaders
               |""".stripMargin
        }.mkString("", "\n", "\n")

        noteRef.setMarkdown(
          s"""Fetched ${emails.size} emails at around ${ZonedDateTime.now()}
             |
             |$formattedEmails
             |
             |# Detailed
             |
             |$formattedHeaders
             |
             |""".stripMargin)

        Tinker.steadily
    }
  }
}
