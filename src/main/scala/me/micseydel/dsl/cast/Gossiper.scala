package me.micseydel.dsl.cast

import akka.actor.typed.ActorRef
import cats.data.NonEmptyList
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TinkerBrainUtil.Listeners
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.dsl.{Sender, SpiritRef, Tinker, TypedMqtt}
import me.micseydel.model.{BaseModel, LargeModel, NotedTranscription, TurboModel}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef
import net.jcazevedo.moultingyaml.*
import spray.json.*

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

object Gossiper {
  sealed trait Message

  final case class Receive(notedTranscription: NotedTranscription) extends Message

  // rudimentary
  final case class SubmitVote(vote: Vote) extends Message

  final case class Vote(noteId: NoteId, confidence: Either[Double, Option[Boolean]], voter: SpiritRef[NonEmptyList[Vote]], voteTime: ZonedDateTime, comments: Option[String]) {
    override def toString = s"""Vote($noteId, $confidence, ${toNormalizedUri(voter.path.toSerializationFormat).drop(35)}, ${voteTime.format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS"))}, $comments)"""

    def voterPathContains(partialPath: String): Boolean = voter.path.toSerializationFormat.contains(partialPath)
  }

  sealed trait Subscription extends Message {
    def subscriber: SpiritRef[NotedTranscription]
  }

  final case class SubscribeAccurate(subscriber: SpiritRef[NotedTranscription]) extends Subscription

  //  final case class SubscribeFast(subscriber: SpiritRef[NotedTranscription]) extends Subscription
  final case class SubscribeHybrid(subscriber: SpiritRef[NotedTranscription]) extends Subscription
  final case class SubscribeTurbo(subscriber: SpiritRef[NotedTranscription]) extends Subscription

  // FIXME: need to add voting; ACK'ing in Chronicler? (combine things for mqtt?)
//  private case class ReceiveMqtt(mqttMessage: TypedMqtt.MqttMessage) extends Message

  private case class ReceiveNotePing(ping: Ping) extends Message

  //

  def apply()(implicit Tinker: Tinker): Ability[Message] =
    AttentiveNoteMakingTinkerer[Message, ReceiveNotePing]("Gossiper", rgb(255, 190, 230), "🗣️", ReceiveNotePing) { (context, noteRef) =>
      context.actorContext.log.info("Started Gossiper")

      //      context.system.mqtt ! TypedMqtt.Subscribe(s"${noteRef.noteId}", context.messageAdapter(ReceiveMqtt).underlying)

//      val generatedMarkdown = noteRef.generateMarkdown() match {
//        case Failure(exception) =>
//          context.actorContext.log.warn(s"Failed to read YAML; will not write Markdown to ${noteRef.noteId}, but will manage listeners and votes", exception)
//          false
//        case Success(value) => value
//      }

      val publishWhisperLargeToMqtt: Boolean = noteRef.getConfig() match {
        case Failure(exception) =>
          context.actorContext.log.warn(s"Something went wrong reading config from disk or deserializing it ${exception.getClass}", exception)
          false
        case Success(None) => false // opt-in to mqtt, rather than opt-out
        case Success(Some(GossiperConfig(publishWhisperLargeToMqtt))) => publishWhisperLargeToMqtt
      }

      behavior(Set.empty, Set.empty, Set.empty, Map.empty, publishWhisperLargeToMqtt
//        , generatedMarkdown
      )(Tinker, noteRef)
    }

  /**
   * @param votesMapping the inner key is the result of normalizedUri() called on the voter's ActorPath
   */
  private def behavior(
                        accurateListeners: Set[SpiritRef[NotedTranscription]],
                        fastListeners: Set[SpiritRef[NotedTranscription]],
                        turboListeners: Set[SpiritRef[NotedTranscription]],
                        votesMapping: Map[NoteId, Map[String, NonEmptyList[Vote]]],
                        publishWhisperLargeToMqtt: Boolean
                        //                        generatedMarkdown: Boolean
                      )(implicit Tinker: Tinker, noteRef: NoteRef): Ability[Message] = Tinker.setup { context =>
    implicit val sender: Sender = Sender(context.self.path)
    implicit val tinkerBrain: ActorRef[TinkerBrain.Message] = context.system.tinkerBrain

//    if (generatedMarkdown) {
//      val formattedMarkdown = votesMapping
//        .toSeq.sortBy(_._2.values.toList.map(_.head).map(_.voteTime))
//        .map { case (noteId, votesOnNote) =>
//          val formattedVotesOnNote = votesOnNote.values
//            .map {
//              case NonEmptyList(Vote(_, confidence, voter, voteTime, maybeComments), theRest) =>
//                val priorVotes = if (theRest.nonEmpty) s"$theRest" else ""
//                MarkdownUtil.listLineWithTimestamp(voteTime, s"${toNormalizedUri(voter.path.toSerializationFormat).drop(35)} -> $confidence", dateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")) +
//                  (if (priorVotes.nonEmpty) {
//                    s"""
//                       |        - comments: $maybeComments
//                       |        - prior votes: $priorVotes""".stripMargin
//                  } else {
//                    s"""
//                       |        - comments: $maybeComments""".stripMargin
//                  })
//            }.mkString("    ", "\n    ", "")
//          val filename = noteId.asString.drop(18).dropRight(3) + "wav"
//          val firstLine = Chronicler.getCaptureTimeFromAndroidAudioPath(filename) match {
//            case Left(msg) =>
//              context.actorContext.log.warn(s"Failed to extract time from noteId $filename: $msg")
//              noteId.toString
//            case Right(time) =>
//              MarkdownUtil.listLineWithTimestampAndRef(time, s"[[${noteId.asString.drop(18)}]]", noteId, dateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")).drop(2)
//          }
//          s"$firstLine\n$formattedVotesOnNote"
//        }.mkString("- ", "\n- ", "\n")
//
//      noteRef.setMarkdown(formattedMarkdown) match {
//        case Failure(exception) => throw exception
//        case Success(NoOp) =>
//      }
//    }

    Tinker.receiveMessage {
      case Receive(notedTranscription) =>
        context.actorContext.log.info(s"Received ${notedTranscription.capture.whisperResult.whisperResultMetadata.model} NotedTranscription with NoteId ${notedTranscription.noteId}; notifying ${accurateListeners.size} listeners")

        notedTranscription.capture.whisperResult.whisperResultMetadata.model match {
          case LargeModel =>
            context.actorContext.log.info(s"""Sending ${notedTranscription.noteId} to ${accurateListeners.size} listeners (large, accurate model) and ${if (publishWhisperLargeToMqtt) "" else "NOT "} publishing to mqtt""")
            accurateListeners *!* notedTranscription
            if (publishWhisperLargeToMqtt) {
              import me.micseydel.model.NotedTranscription.NotedTranscriptionJsonProtocol.notedTranscriptionFormat
              context.system.mqtt ! TypedMqtt.Publish("[[Gossiper]]/publish/WhisperLarge", notedTranscription.toJson.compactPrint.getBytes)
            } else {
              context.actorContext.log.info("not publishing to mqtt, config not set to true")
            }
          case TurboModel =>
            context.actorContext.log.info(s"Sending ${notedTranscription.noteId} to ${accurateListeners.size} listeners (turbo, compromise model)")
            turboListeners *!* notedTranscription
          case BaseModel =>
            context.actorContext.log.info(s"Sending ${notedTranscription.noteId} to ${fastListeners.size} listeners (base, fast model)")
            fastListeners *!* notedTranscription
        }

        Tinker.steadily

      case SubscribeAccurate(subscriber) =>
        context.actorContext.log.debug(s"Adding ${subscriber.path} to accurate subscribers")
        behavior(accurateListeners + subscriber, fastListeners, turboListeners, votesMapping, publishWhisperLargeToMqtt
//          , generatedMarkdown
        )

      case SubscribeHybrid(subscriber) =>
        context.actorContext.log.debug(s"Adding ${subscriber.path} to both fast and accurate subscribers")
        behavior(accurateListeners + subscriber, fastListeners + subscriber, turboListeners, votesMapping, publishWhisperLargeToMqtt
//          , generatedMarkdown
        )

      case SubscribeTurbo(subscriber) =>
        context.actorContext.log.debug(s"Adding ${subscriber.path} to turbo")
        behavior(accurateListeners, fastListeners, turboListeners + subscriber, votesMapping, publishWhisperLargeToMqtt
//          , generatedMarkdown
        )

      // experiment; voting on *notes* may generalize well, but requires receivers to interpret arbitrarily
      case SubmitVote(newVote) =>
//        context.actorContext.log.info(s"new vote $newVote") // FIXME: chatty, but it seemed like stuff was being dropped...
        val normalizedUri: String = toNormalizedUri(newVote.voter.path.toSerializationFormat)
        votesMapping.get(newVote.noteId) match {
          case None =>
            val updatedVotes = votesMapping.updated(newVote.noteId, Map(normalizedUri -> NonEmptyList.of(newVote)))
            behavior(accurateListeners, fastListeners, turboListeners, updatedVotes, publishWhisperLargeToMqtt
//              , generatedMarkdown
            )
          case Some(voterToVotesMap) =>
            voterToVotesMap.get(normalizedUri) match {
              case None =>
                voterToVotesMap.values.map(_.head).toList match {
                  case head :: tail =>
                    val cachedVotes: NonEmptyList[Vote] = NonEmptyList(head, tail)
                    newVote.voter !!! cachedVotes
                  case Nil =>
                }

                val updatedVotes: Map[String, NonEmptyList[Vote]] = voterToVotesMap.updated(normalizedUri, NonEmptyList.of(newVote))
                behavior(accurateListeners, fastListeners, turboListeners, votesMapping.updated(newVote.noteId, updatedVotes), publishWhisperLargeToMqtt
//                  , generatedMarkdown
                )

              case Some(votes) =>
                val latestVote = votes.head
                if (latestVote.voteTime == newVote.voteTime) {
                  context.actorContext.log.warn(s"Ignoring vote on ${newVote.noteId} by $normalizedUri, already seen the timestamp for the noteId by the voter")
                  Tinker.steadily
                } else {
                  val votersToNotify = voterToVotesMap.values.map(_.head).filterNot(sameVoters(latestVote, _))
                  for (cachedVoter <- votersToNotify.map(_.voter)) {
                    cachedVoter !!! NonEmptyList.of(newVote)
                  }

                  val updatedVotesListForVoter = newVote :: votes
                  val updatedVotesMap = voterToVotesMap.updated(normalizedUri, updatedVotesListForVoter)
                  val updatedVotesMapping = votesMapping.updated(newVote.noteId, updatedVotesMap)

                  behavior(accurateListeners, fastListeners, turboListeners, updatedVotesMapping, publishWhisperLargeToMqtt
//                    , generatedMarkdown
                  )
                }
            }
        }

//      case ReceiveMqtt(mqttMessage) =>
//        val subscriberTopic = new String(mqttMessage.payload)
//        context.actorContext.log.info(s"Received mqtt subscriber $subscriberTopic")
//        behavior(
//          accurateListeners,
//          fastListeners, turboListeners, votesMapping, publishWhisperLargeToMqtt
////          , generatedMarkdown
//        )

      case ReceiveNotePing(_) =>
        val updatedPublishWhisperLargeToMqtt = noteRef.getConfig() match {
          case Failure(exception) =>
            context.actorContext.log.warn(s"Refreshing publishWhisperLargeToMqtt failed unexpected, retaining $publishWhisperLargeToMqtt (${exception.getClass})", exception)
            publishWhisperLargeToMqtt // still retain
          case Success(None) => publishWhisperLargeToMqtt // retain existing behavior
          case Success(Some(config)) => config.publishWhisperLargeToMqtt
        }

        behavior(
          accurateListeners, fastListeners, turboListeners, votesMapping,
          publishWhisperLargeToMqtt = updatedPublishWhisperLargeToMqtt
        )
    }
  }

  private def sameVoters(firstVote: Vote, secondVote: Vote): Boolean =
    toNormalizedUri(firstVote.voter.path.toSerializationFormat) == toNormalizedUri(secondVote.voter.path.toSerializationFormat)

  def toNormalizedUri(uri: String): String = {
    val t = uri.split("/").toList.reverse.dropWhile(_.startsWith("$")).reverse.mkString("/")
    t.split('#').toList match {
      case List(wanted, _) => wanted
      case _ => t
    }
  }

  //

  private case class GossiperConfig(publishWhisperLargeToMqtt: Boolean)

  private object YamlProtocol extends DefaultYamlProtocol {
    implicit val configYamlFormat: YamlFormat[GossiperConfig] = yamlFormat1(GossiperConfig)
  }

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    def getConfig(): Try[Option[GossiperConfig]] = {
      import YamlProtocol.configYamlFormat
      noteRef.read()
        .map(_.maybeFrontmatter.map(frontmatter => Try(frontmatter.parseYaml.convertTo[GossiperConfig])))
        .flatMap {
          case Some(value) =>
            value.map(Some(_))

          case None =>
            Success(None)
        }
    }
  }
}
