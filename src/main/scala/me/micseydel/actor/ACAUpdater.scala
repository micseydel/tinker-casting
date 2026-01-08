package me.micseydel.actor

import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.actor.google.GmailActor
import me.micseydel.actor.google.GmailActor.Email
import me.micseydel.app.GoogleSlideUpdater
import me.micseydel.app.GoogleSlideUpdater.Replacement
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import net.jcazevedo.moultingyaml.*

import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object ACAUpdater {
  sealed trait Message

  private final case class ReceiveEmails(emails: Seq[GmailActor.Email]) extends Message

  private final case class ReceiveSlidesActor(slidesActor: Option[SpiritRef[GoogleSlideUpdater.Message]]) extends Message

  private final case object CheckForSlidesOneMoreTime extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("ACAUpdater", TinkerColor.random(), "🙏") { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context
    implicit val nr: NoteRef = noteRef

    noteRef.readValidatedConfig() match {
      case Validated.Valid(config: ACAUpdaterConfig) =>
        implicit val c: ACAUpdaterConfig = config

        context.system.operator !! Operator.SubscribeGmail(context.messageAdapter(ReceiveEmails))
        context.system.operator !! Operator.FetchGoogleSlides(context.messageAdapter(ReceiveSlidesActor))

        implicit val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()

        initializing(retryDone = false)

      case Validated.Invalid(failures) =>
        context.actorContext.log.warn(s"Failures: $failures")
        Tinker.ignore
    }
  }

  private def initializing(retryDone: Boolean)(implicit Tinker: Tinker, noteRef: NoteRef, config: ACAUpdaterConfig, timeKeeper: SpiritRef[TimeKeeper.Message]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context
    Tinker.receiveMessage {
      case ReceiveEmails(emails) =>
        checkForMatchingEmails(emails) match {
          case Validated.Valid(maybeMatch) =>
            maybeMatch match {
              case Some(emailMatch: EmailMatch) =>
                noteRef.writeMatchingEmailToDisk(emailMatch) match {
                  case Failure(exception) => context.actorContext.log.warn(s"Failed to write matching email to disk! ${emailMatch.title}", exception)
                  case Success(NoOp) =>
                }
              case None => // ignore
            }

          case Validated.Invalid(e) =>
            context.actorContext.log.warn(s"Something went wrong while checking new emails: $e")
        }

        Tinker.steadily

      case ReceiveSlidesActor(Some(slidesActor)) => initialized(slidesActor)
      case ReceiveSlidesActor(None) =>
        if (!retryDone) {
          val secondsToWait = 20
          context.actorContext.log.warn(s"No slides actor yet, will check once more in $secondsToWait seconds...")
          timeKeeper !! TimeKeeper.RemindMeIn(secondsToWait.seconds, context.self, CheckForSlidesOneMoreTime, None)
        } else {
          context.actorContext.log.error("Fetching slides failed, not retrying again")
        }
        initializing(retryDone = true)

      case CheckForSlidesOneMoreTime =>
        context.actorContext.log.warn("Requesting slides actor one more time...")
        context.system.operator !! Operator.FetchGoogleSlides(context.messageAdapter(ReceiveSlidesActor))
        Tinker.steadily
    }
  }

  private def initialized(slidesActor: SpiritRef[GoogleSlideUpdater.Message])(implicit Tinker: Tinker, noteRef: NoteRef, config: ACAUpdaterConfig): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[?] = context

    context.actorContext.log.info("Initialized!")
    slidesActor !! GoogleSlideUpdater.SaveSlideInfoToNote(config.presentationId)

    Tinker.receiveMessage {
      case ReceiveEmails(emails) =>
        checkForMatchingEmails(emails) match {
          case Validated.Valid(maybeMatch) =>
            maybeMatch match {
              case Some(emailMatch@EmailMatch(title, brb, body, footer)) =>
                context.actorContext.log.info(s"Match for $title! Updating slide...")
                noteRef.writeMatchingEmailToDisk(emailMatch) match {
                  case Failure(exception) => context.actorContext.log.warn(s"Failed to write matching email to disk! ${emailMatch.title}", exception)
                  case Success(NoOp) =>
                }
                slidesActor !! GoogleSlideUpdater.ReplaceText(config.presentationId, List(
                  Replacement(config.replacements.title, title, fontSize = 38, italics = false),
                  Replacement(config.replacements.brb, brb, fontSize = 14, italics = true),
                  Replacement(config.replacements.body, body, fontSize = 14, italics = false),
                  Replacement(config.replacements.footer, footer, fontSize = 14, italics = true),
                ))
              case None => context.actorContext.log.debug(s"No matching emails out of ${emails.size}")
            }

          case Validated.Invalid(e) =>
            context.actorContext.log.warn(s"Something went wrong while checking new emails: $e")
        }

        Tinker.steadily

      case ReceiveSlidesActor(Some(slidesActor)) => initialized(slidesActor)
      case ReceiveSlidesActor(None)| CheckForSlidesOneMoreTime => Tinker.steadily
    }
  }

  case class EmailMatch(
                         title: String,
                         brb: String,
                         body: String,
                         footer: String
                       )

  private def checkForMatchingEmails(emails: Seq[Email])(implicit config: ACAUpdaterConfig): ValidatedNel[String, Option[EmailMatch]] = {
    val matches = emails.filter(_.subject == config.subject).map(_.body).flatMap { rawBody =>
      rawBody.split("\n").toList.map(_.trim) match {
        case _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: day :: _ :: _ :: _ :: topic :: _ :: _ :: brb :: _ :: theRest =>
          val subjectWithoutPrefix = if (topic.startsWith(config.topicPrefix)) {
            topic.drop(config.topicPrefix.length)
          } else topic
          val fixedSubject = if (subjectWithoutPrefix.endsWith(config.topicSuffix)) {
            subjectWithoutPrefix.dropRight(config.topicSuffix.length)
          } else subjectWithoutPrefix

          val bodyAndFooter = theRest.takeWhile(!_.startsWith("Copyright ©")).dropRight(1)
          bodyAndFooter.lastOption.map { footer =>
            val bodyLines = bodyAndFooter.dropRight(2)
            val topic = s"$day - $fixedSubject"
            EmailMatch(topic, brb, bodyLines.mkString("\n"), footer)
          }

        case _ => None
      }
    }

    matches match {
      case List(singleMatch) => Some(singleMatch).validNel
      case Nil => None.validNel
      case other => s"Expected none or one but got: $other".invalidNel
    }
  }

  case class Replacements(
                           title: String,
                           brb: String,
                           body: String,
                           footer: String,
                         )

  case class ACAUpdaterConfig(
                               slidesUrl: String,
                               subject: String,
                               topicPrefix: String,
                               topicSuffix: String,
                               replacements: Replacements
                             ) {
    def presentationId: String = slidesUrl.drop(ExpectedURLPrefix.length).takeWhile(_ != '/')
  }

  private object YamlProtocol extends DefaultYamlProtocol {
    implicit val replacementsYamlFormat: YamlFormat[Replacements] = yamlFormat4(Replacements)
    implicit val configYamlFormat: YamlFormat[ACAUpdaterConfig] = yamlFormat5(ACAUpdaterConfig)
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def readValidatedConfig(): ValidatedNel[String, ACAUpdaterConfig] = {
      noteRef.readNote() match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(note) =>
          import YamlProtocol.configYamlFormat
          val validatedConfig: ValidatedNel[String, ACAUpdaterConfig] = note.maybeFrontmatter
            .map(frontMatter => Try(frontMatter.parseYaml.convertTo[ACAUpdaterConfig])) match {
            case Some(value) =>
              value match {
                case Failure(exception) => Common.getStackTraceString(exception).invalidNel
                case Success(value) => value.validNel
              }
            case None => "no frontmatter found".invalidNel
          }

          validatedConfig
      }
    }

    def writeMatchingEmailToDisk(matching: EmailMatch)(implicit context: TinkerContext[?]): Try[NoOp.type] = {
      noteRef.setMarkdown(
        s"""- Generated: ${context.system.clock.now()}
           |
           |# ${matching.title}
           |
           |**${matching.brb}**
           |
           |
           |${matching.body}
           |
           |
           |*${matching.footer}*
           |""".stripMargin
      )
    }
  }

  private val ExpectedURLPrefix = "https://docs.google.com/presentation/d/"
}
