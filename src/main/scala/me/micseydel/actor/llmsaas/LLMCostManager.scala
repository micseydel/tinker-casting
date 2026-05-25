package me.micseydel.actor.llmsaas

import cats.data.{Validated, ValidatedNel}
import cats.implicits.catsSyntaxValidatedId
import me.micseydel.Common.RichNoteRef
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.llmsaas.LLMCostManager.{LLMCostTrackingFailure, LLMCostTrackingResult, LLMCostTrackingSuccess, TrackUse}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.{AttentiveNoteMakingTinkerer, NoteMakingTinkerer}
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor, TinkerContext}
import me.micseydel.util.JsonUtil
import me.micseydel.vault.persistence.NoteRef
import me.micseydel.{Common, NoOp}
import net.jcazevedo.moultingyaml.*
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, JsonFormat, RootJsonFormat, enrichAny}

import java.time.ZonedDateTime
import scala.util.{Failure, Success, Try}

object LLMCostManager {
  sealed trait Message

  final case class TrackUse(promptId: String, time: ZonedDateTime, provider: String, model: String, result: LLMCostTrackingResult) extends Message

  // FIXME just for the models - maybe create wrappers, with the existing ones as API?
  private case class ReceiveAnthropic(response: ClaudeActor.Response) extends Message
  private case class ReceiveOpenAI(response: OpenAIActor.Response) extends Message

  private case class NotePing(ping: Ping) extends Message

  ///

  sealed trait LLMCostTrackingResult

  final case class LLMCostTrackingFailure(message: String) extends LLMCostTrackingResult
  final case class LLMCostTrackingSuccess(inputTokens: Int, outputTokens: Int) extends LLMCostTrackingResult

  ///

  val NoteName = "LLMCostManager"
  val Emoji = "💸"

  def apply(claude: SpiritRef[ClaudeActor.Command], openai: SpiritRef[OpenAIActor.Command])(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, NotePing](NoteName, TinkerColor.random(), Emoji, NotePing) { (context, noteRef) =>
      implicit val tc: TinkerContext[?] = context
      implicit val nr: NoteRef = noteRef

      noteRef.refreshMarkdown()

      val ledger = context.cast(LLMCostManagerHistory(), "LLMCostManagerHistory")

      import YamlProtocol.frontMatterYamlFormat

      Tinker.receiveMessage {
        case ReceiveAnthropic(response) =>
          context.actorContext.log.info("Integrating latest anthropic models")
          response match {
            case ClaudeActor.ModelsSuccess(models) =>
              noteRef.readValidatedFrontmatter() match {
                case Validated.Valid(a: FrontMatter) =>
                  val pretty = a.integrateAnthropic(models).toYaml.print
                  context.actorContext.log.warn(s"About to write: ${pretty.take(20)}...")
                  noteRef.setFrontMatterRaw(pretty) match {
                    case Failure(exception) => context.actorContext.log.warn("failed to set frontmatter", exception)
                    case Success(_) =>
                  }
                case Validated.Invalid(errors) =>
                  context.actorContext.log.warn(s"Something(s) went wrong reading frontmatter: $errors")
              }

            case ClaudeActor.CompletionSuccess(text, usage) =>
              if (context.actorContext.log.isDebugEnabled) {
                context.actorContext.log.warn(s"Unexpected CompletionSuccess from Claude, usage: $usage")
              } else {
                context.actorContext.log.warn(s"Unexpected CompletionSuccess from Claude, usage: $usage; completion = $text")
              }

            case ClaudeActor.Failure(reason) =>
              context.actorContext.log.warn(s"Claude models request failed: $reason")
          }

          Tinker.steadily

        case ReceiveOpenAI(response) =>
          context.actorContext.log.info("Integrating latest openai models")
          response match {
            case OpenAIActor.ModelsSuccess(models) =>
              noteRef.readValidatedFrontmatter() match {
                case Validated.Valid(a: FrontMatter) =>
                  val pretty = a.integrateOpenAI(models).toYaml.print
                  context.actorContext.log.warn(s"About to write: ${pretty.take(20)}...")
                  noteRef.setFrontMatterRaw(pretty) match {
                    case Failure(exception) => context.actorContext.log.warn(s"failed to set frontmatter", exception)
                    case Success(_) =>
                  }
                case Validated.Invalid(errors) =>
                  context.actorContext.log.warn(s"Something(s) went wrong reading frontmatter: $errors")
              }

            case OpenAIActor.CompletionSuccess(text, usage) =>
              if (context.actorContext.log.isDebugEnabled) {
                context.actorContext.log.warn(s"Unexpected CompletionSuccess from OpenAI, usage: $usage")
              } else {
                context.actorContext.log.warn(s"Unexpected CompletionSuccess from OpenAI, usage: $usage; completion = $text")
              }

            case OpenAIActor.Failure(reason) =>
              context.actorContext.log.warn(s"OpenAI models request failed: $reason")
          }
          Tinker.steadily

        case NotePing(NoOp) =>
          if (noteRef.checkBoxIsChecked()) {
            claude !! ClaudeActor.ListModels(context.messageAdapter(ReceiveAnthropic).underlying)
            openai !! OpenAIActor.ListModels(context.messageAdapter(ReceiveOpenAI).underlying)
            context.actorContext.log.info("Refreshing models")
            noteRef.refreshMarkdown()
          }

          Tinker.steadily

        case track@TrackUse(_, _, _, _, _) =>
          ledger !! track
          Tinker.steadily
      }
    }

  //

  /**
   * Both are in CENTS per MTokens, e.g. $2.50 per 1M tokens is 250
   *
   * OpenAI has complicated context size stuff, 272k tokens or soemthing
   */
  case class Entry(inputCost: Int, outputCost: Int)

  case class Entries(anthropic: Map[String, Entry], openai: Map[String, Entry]) {
    def integrateAnthropic(models: List[ModelInfo]): Entries = {
      this.copy(anthropic = integrationHelper(models, anthropic))
    }

    def integrateOpenAI(models: List[ModelInfo]): Entries = {
      this.copy(openai = integrationHelper(models, openai))
    }

    private def integrationHelper(models: List[ModelInfo], mapping: Map[String, Entry]): Map[String, Entry] = {
      models.foldRight(mapping) { (nextUp, soFar) =>
        soFar.updatedWith(nextUp.id) {
          case existing@Some(Entry(_, _)) => existing
          case None => Some(Entry(-1, -1))
        }
      }
    }
  }

  case class FrontMatter(
                          entries: Entries,
                          latestRawAnthropic: String,
                          latestRawOpenai: String
                        ) {
    import me.micseydel.actor.llmsaas.SharedDomainJsonProtocol.modelInfoListJsonFormat

    def integrateAnthropic(models: List[ModelInfo]): FrontMatter = {
      this.copy(
        entries = entries.integrateAnthropic(models),
        latestRawAnthropic = models.toJson.compactPrint
      )
    }

    def integrateOpenAI(models: List[ModelInfo]): FrontMatter = {
      this.copy(
        entries = entries.integrateOpenAI(models),
        latestRawOpenai = models.toJson.compactPrint
      )
    }
  }

  private object YamlProtocol extends DefaultYamlProtocol {
    implicit val replacementsYamlFormat: YamlFormat[Entry] = yamlFormat2(Entry)
    implicit val entriesYamlFormat: YamlFormat[Entries] = yamlFormat2(Entries)
    implicit val frontMatterYamlFormat: YamlFormat[FrontMatter] = yamlFormat3(FrontMatter)
  }

  //

  private implicit class RichLLMCostManagerNoteRef(val noteRef: NoteRef) extends AnyVal {
    import YamlProtocol.frontMatterYamlFormat

    def refreshMarkdown(): NoOp.type = noteRef.setMarkdownOrThrow(
      s"""- [ ] Re-fetch models
         |- ...then: update manually using-
         |    - https://claude.com/pricing#api
         |    - https://developers.openai.com/api/docs/pricing
         |- see also: [[LLMCostManagerHistory]]
         |""".stripMargin
    )

    def readValidatedFrontmatter(): ValidatedNel[String, FrontMatter] = {
      noteRef.readNote() match {
        case Failure(exception) => s"Something went wrong reading from disk: ${Common.getStackTraceString(exception)}".invalidNel
        case Success(note) =>
          val validatedConfig: ValidatedNel[String, FrontMatter] = note.maybeFrontmatter
            .map { frontMatter =>
              Try(frontMatter.parseYaml.convertTo[FrontMatter])
            } match {
            case Some(value) =>
              value match {
                case Failure(exception) => Common.getStackTraceString(exception).invalidNel
                case Success(value) => value.validNel
              }
            case None =>
              // FIXME log
              FrontMatter(Entries(Map.empty, Map.empty), "", "").validNel
          }

          validatedConfig
      }
    }
  }
}

private object LLMCostManagerHistory {

  private val NoteName = "LLMCostManagerHistory"
  private val Emoji = "📒"

  def apply()(implicit Tinker: Tinker): Ability[TrackUse] = NoteMakingTinkerer(NoteName, TinkerColor.random(), Emoji) { (context, noteRef) =>
    import LLMCostManagerHistoryJsonProtocol.trackUseJsonFormat
    Tinker.receiveMessage {
      case track@TrackUse(time, _, _, _, _) =>
        val serialized = track.toJson.compactPrint
        // FIXME: should de-dupe this, since it can be adding an updated record but the old one is kept
        noteRef.append(s"- $serialized\n") match {
          case Failure(exception) => context.actorContext.log.warn(s"Uh oh failed to append cost for inference that started at $time; $serialized", exception)
          case Success(NoOp) =>
        }
        Tinker.steadily
    }
  }

  private object LLMCostManagerHistoryJsonProtocol extends DefaultJsonProtocol {
    import JsonUtil.ZonedDateTimeJsonFormat

    implicit val llmCostTrackingFailureJsonFormat: JsonFormat[LLMCostTrackingFailure] = jsonFormat1(LLMCostTrackingFailure)
    implicit val llmCostTrackingSuccessJsonFormat: JsonFormat[LLMCostTrackingSuccess] = jsonFormat2(LLMCostTrackingSuccess)

    implicit object LLMCostTrackingResultJsonFormat extends RootJsonFormat[LLMCostTrackingResult] {
      def write(wm: LLMCostTrackingResult): JsString = JsString(wm.toString)

      def read(value: JsValue): LLMCostTrackingResult = {
        value.asJsObject.getFields("type") match {
          case Seq(JsString("LLMCostTrackingFailure")) => value.convertTo[LLMCostTrackingFailure]
          case Seq(JsString("LLMCostTrackingSuccess")) => value.convertTo[LLMCostTrackingSuccess]
          case other => throw DeserializationException(s"Unknown type, expected Seq(JsString(_)) for one of {TranscriptionEvent, ReceiveAranetResult} but got $other")
        }
      }
    }

    implicit val trackUseJsonFormat: JsonFormat[TrackUse] = jsonFormat5(TrackUse)
  }
}
