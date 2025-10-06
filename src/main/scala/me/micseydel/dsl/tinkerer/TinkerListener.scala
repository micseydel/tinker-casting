package me.micseydel.dsl.tinkerer

import akka.actor.typed.Scheduler
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import me.micseydel.actor.RasaActor
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.Gossiper.Subscription
import me.micseydel.dsl.cast.chronicler.Chronicler.ListenerAcknowledgement
import me.micseydel.dsl.{EnhancedTinker, SpiritRef, TinkerContext}
import me.micseydel.model.{NotedTranscription, RasaResult}
import me.micseydel.util.StringImplicits.RichString
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object TinkerListener {
  sealed trait Message

  case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message

  sealed trait ListenerResult

  case object Ignored extends ListenerResult

  case class Acknowledged(listenerAcknowledgement: ListenerAcknowledgement) extends ListenerResult

  // FIXME: TinkerListener is NOT clearly in userspace, should fix
  def simpleStateless(wrapper: SpiritRef[NotedTranscription] => Subscription)(behavior: (TinkerContext[_], NotedTranscription) => ListenerResult)(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinker.setup { context =>
    implicit val tc: TinkerContext[_] = context

    Tinker.userExtension.gossiper !! wrapper(context.messageAdapter(TranscriptionEvent))

    Tinker.receiveMessage {
      case TranscriptionEvent(notedTranscription) =>
        behavior(context, notedTranscription) match {
          case Ignored =>
          case Acknowledged(listenerAcknowledgement) =>
            Tinker.userExtension.chronicler !! listenerAcknowledgement
        }

        Tinker.steadily
    }
  }
}

object RasaAnnotatingListener {
  sealed trait Message
  private case class TranscriptionEvent(notedTranscription: NotedTranscription) extends Message

  def apply(model: String, subscription: SpiritRef[NotedTranscription] => Gossiper.Subscription, listener: SpiritRef[RasaAnnotatedNotedTranscription], replacementCandidate: Option[String] = None)(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    Tinker.userExtension.gossiper !! subscription(context.messageAdapter(TranscriptionEvent))

    implicit val scheduler: Scheduler = context.system.actorSystem.scheduler
    implicit val duration: FiniteDuration = 30.seconds // FIXME: hopefully can be faster, or more likely, replaced
    implicit val timeout: Timeout = Timeout(duration)

//    val maybeExperiment = replacementCandidate.map { replacement =>
//      context.cast(RasaExperiment(model, replacement), "RasaExperiment")
//    }

    def getRasaResultFut(rawText: String, modelChoice: String): Future[RasaResult] = {
      Await.ready[RasaResult](Tinker.userExtension.rasa.underlying.ask(RasaActor.GetRasaResult(rawText, modelChoice, _)), duration)
    }

    Tinker.receiveMessage {
      case TranscriptionEvent(notedTranscription) =>
        val rawText = notedTranscription.capture.whisperResult.whisperResultContent.text

//        val maybeComparisonFut: Option[Future[RasaResult]] = replacementCandidate.map(getRasaResultFut(rawText, _))

        val maybeRasaResult: Option[RasaResult] = if (rawText.wordCount < 30) {
          getRasaResultFut(rawText, model).value match {
            case None =>
              context.actorContext.log.warn(s"It looks like the future for ${notedTranscription.noteId} was empty")
              None
            case Some(value) =>
              value match {
                case Failure(exception) =>
                  context.actorContext.log.warn(s"Fetching Rasa result for ${notedTranscription.noteId} failed", exception)
                  None
                case Success(rasaResult) =>
//                  for {
//                    experiment <- maybeExperiment
//                    comparisonFut <- maybeComparisonFut
//                  } {
//                    implicit val ec: ExecutionContextExecutor = context.system.actorSystem.executionContext
//                    context.actorContext.log.debug(s"Setting onComplete for ${notedTranscription.noteId}")
//                    // FIXME: lazy
//                    comparisonFut.onComplete {
//                      case Failure(exception) =>
//                        exception.printStackTrace()
//                      case Success(toCompare) =>
//                        experiment !! RasaExperiment.Receive(notedTranscription, rasaResult, toCompare)
//                    }
//                  }
                  Some(rasaResult)
              }
          }
        } else {
          None
        }

        context.actorContext.log.debug(s"Messaging listener $maybeRasaResult")
        listener !! RasaAnnotatedNotedTranscription(notedTranscription, maybeRasaResult)

        Tinker.steadily
    }
  }

  //

  case class RasaAnnotatedNotedTranscription(notedTranscription: NotedTranscription, maybeRasaResult: Option[RasaResult])

  case object MessageListJsonProtocol extends DefaultJsonProtocol {
    import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat
    import me.micseydel.model.RasaResultProtocol.rasaResultFormat
    implicit val RasaAnnotatedNotedTranscriptionJsonFormat: RootJsonFormat[RasaAnnotatedNotedTranscription] =
      jsonFormat2(RasaAnnotatedNotedTranscription)
  }
}

//object RasaExperiment {
//  sealed trait Message
//  final case class Receive(notedTranscription: NotedTranscription, rasaResult: RasaResult, rasaResultToCompare: RasaResult) extends Message
//
//  def apply(model: String, testModel: String)(implicit Tinker: Tinker): Ability[Message] = {
//    val noteName = s"Rasa Experiment comparing $model and $testModel"
//    NoteMakingTinkerer(noteName, TinkerColor(100, 50, 50), "👨‍🔬") { (context, noteRef) =>
//      context.actorContext.log.debug(s"Created [[$noteName]]")
//      Tinker.receiveMessage {
//        case Receive(notedTranscription, rasaResult, rasaResultToCompare) =>
//          context.actorContext.log.debug(s"Received ${notedTranscription.noteId}, processing...")
//          val elaboration = (rasaResult, rasaResultToCompare) match {
//            case (
//              RasaResult(reference_entities, reference_intent, reference_intent_ranking, _, _),
//              RasaResult(entities, intent, intent_ranking, _, _)
//              ) =>
//              if (reference_intent.name == intent.name) {
//                // FIXME: this is sloppy, an entity can appear multiple times
//                if (reference_entities.map(e => e.entity -> e.value).toMap == entities.map(e => e.entity -> e.value).toMap) {
//                  s"    - Intent (${reference_intent.name}) and entities match"
//                } else {
//                  val entitiesElaboration = formattedEntitiesMarkdown(model, reference_entities)
//                  s"    - Intent (${reference_intent.name}) matches but entities differ\n$entitiesElaboration"
//                }
//              } else {
//                s"""    - Intent ${intent.name} (${intent.confidence}) did not match reference intent ${reference_intent.name} (${reference_intent.confidence})
//                   |    - $model $reference_intent_ranking
//                   |    - $testModel $intent_ranking
//                   |""".stripMargin
//              }
//          }
//
//          val firstLine = MarkdownUtil.listLineWithTimestampAndRef(notedTranscription.capture.captureTime, StringUtil.truncateText(notedTranscription.capture.whisperResult.whisperResultContent.text), notedTranscription.noteId)
//          noteRef.appendOrThrow(
//            s"""$firstLine
//               |$elaboration
//               |""".stripMargin)
//          Tinker.steadily
//      }
//    }
//  }
//
//  private def formattedEntitiesMarkdown(model: String, entities: List[Entity]): String = {
//    val entitiesFormatted = entities.map {
//      case Entity(confidence_entity, confidence_group, _, entity, _, group, _, value) =>
//        s"        - $entity -> $value ($confidence_entity; $group, $confidence_group)"
//    }.mkString("\n")
//    s"""    - $model
//       |$entitiesFormatted
//       |""".stripMargin
//  }
//}
