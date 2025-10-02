package me.micseydel.actor.airgradient


import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.{DailyMarkdownFromPersistedMessagesActor, DailyNotesRouter}
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.{DoubleSeries, IntSeries}
import me.micseydel.vault.{Note, NoteId}
import me.micseydel.vault.persistence.NoteRef

import java.io.FileNotFoundException
import java.time.LocalDate
import scala.concurrent.duration.{DurationInt, DurationLong}
import scala.util.{Failure, Success}

object AirGradientActor {
  sealed trait Message

  final case class Subscribe(subscriber: SpiritRef[AirGradientSensorResult]) extends Message
  final case class DoFetchNow() extends Message

  private case class ReceivePing(ping: Ping) extends Message

  private case class ReceiveAirGradientSensorData(data: AirGradientSensorData) extends Message

  def apply(api: SpiritRef[AirGradientApiActor.Message], serial: String, nickname: Option[String])(implicit Tinker: Tinker): (String, Ability[Message]) = {
    val baseNoteName = s"Air Gradient measurements - $serial"
    // FIXME: return something that includes the nickname
    baseNoteName -> AttentiveNoteMakingTinkerer[Message, ReceivePing](baseNoteName, TinkerColor(145, 96, 220), "😮‍💨", ReceivePing, Some("_actor_notes")) { (context, noteRef) =>
      // FIXME: nickname as alias!
      implicit val c: TinkerContext[?] = context

      val initialPollingInterval = noteRef.read() match {
        case Failure(_: FileNotFoundException) =>
          noteRef.setTo(Note(s"initializing as of ${context.system.clock.now()}", Some("polling_interval_minutes: 10")))
          10.minutes
        case Failure(exception) => throw exception
        case Success(note) =>
          note.yamlFrontMatter.toOption.flatMap(_.get("polling_interval_minutes")) match {
            case Some(value: Int) =>
              context.actorContext.log.info(s"Using polling_interval_minutes $value from disk")
              value.minutes
            case other =>
              val default = 10.minutes
              context.actorContext.log.info(s"Using polling_interval_minutes default ${default.toMinutes} (ignoring $other)")
              default
          }
      }

      implicit val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[AirGradientSensorData]]] = context.cast(DailyNotesRouter(
        baseNoteName,
        s"airgradient_$serial",
        AirGradientJsonFormat.airGradientSensorDataJsonFormat,
        AirGradientDailyMarkdown.apply
      ), "DailyNotesRouter")

      implicit val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
      timeKeeper !! TimeKeeper.RemindMeEvery(initialPollingInterval, context.self, DoFetchNow(), Some(classOf[DoFetchNow]))
      context.self !! DoFetchNow()

      implicit val a: SpiritRef[AirGradientApiActor.Message] = api
      implicit val n: NoteRef = noteRef
      implicit val dailyNoteName: LocalDate => String = day => s"$baseNoteName ($day)"

      behavior(initialPollingInterval.toMinutes, Set.empty)
    }
  }

  private def behavior(pollingIntervalMinutes: Long, subscribers: Set[SpiritRef[AirGradientSensorResult]])(implicit Tinker: Tinker, noteRef: NoteRef,
                                                     dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[AirGradientSensorData]]],
                                                     api: SpiritRef[AirGradientApiActor.Message],
                                                     timeKeeper: SpiritRef[TimeKeeper.Message],
                                                     dailyNoteName: LocalDate => String): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[?] = context
    message match {
      case DoFetchNow() =>
        context.actorContext.log.info("Doing a fetch now")
        api !! AirGradientApiActor.Request(context.messageAdapter(ReceiveAirGradientSensorData))
        Tinker.steadily

      case ReceivePing(_) =>
        noteRef.readNote() match {
          case Failure(exception) =>
            context.actorContext.log.error("Failure reading from disk", exception)
            Tinker.steadily

          case Success(note) =>
            note.yamlFrontMatter match {
              case Failure(exception) =>
                context.actorContext.log.warn(s"failed to parse frontmatter ${note.maybeFrontmatter}", exception)
                Tinker.steadily
              case Success(frontmatter) =>

                val refreshNow = if (note.markdown.startsWith("initializing")) {
                  false
                } else {
                  note.markdown(3) match {
                    case 'x' => true
                    case ' ' => false
                    case other =>
                      context.actorContext.log.warn(s"Unexpected char $other for ${note.markdown}")
                      false
                  }
                }

                val updateIntervalTo: Option[Long] =
                  frontmatter.get("polling_interval_minutes") match {
                    case Some(value: Int) if value > 0 =>
                      if (value == pollingIntervalMinutes) {
                        context.actorContext.log.debug("polling_interval_minutes, ignoring")
                        None
                      } else {
                        context.actorContext.log.info(s"polling_interval_minutes is now $value")
                        Some(value.toLong)
                      }
                    case other =>
                      context.actorContext.log.warn(s"Ignoring polling_interval_minutes $other")
                      None
                  }

                if (refreshNow) {
                  api !! AirGradientApiActor.Request(context.messageAdapter(ReceiveAirGradientSensorData))
                }

                updateIntervalTo match {
                  case Some(updatedInterval) =>
                    context.actorContext.log.info(s"Updating interval from $pollingIntervalMinutes -> $updatedInterval (minutes)")
                    timeKeeper !! TimeKeeper.RemindMeEvery(updatedInterval.minutes, context.self, DoFetchNow(), Some(classOf[DoFetchNow]))
                    behavior(updatedInterval, subscribers)
                  case None => Tinker.steadily
                }
            }
        }

      case ReceiveAirGradientSensorData(data@AirGradientSensorData(pm02Count, pm10Count, pm02Compensated, rco2, tvocIndex)) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(
          DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(data),
          context.system.clock.now() // FIXME: hacky, prevents downstream from de-duping properly
        )

        for (subscriber <- subscribers) {
          subscriber !! AirGradientSensorResult(noteRef.noteId, data)
        }

//        val subscriberList = subscribers.map { s =>
//          s"  - `${s.path.toString.drop(24).takeWhile(_ != '$')}`"
//        }.mkString("\n")

        val frontmatter: Map[String, Object] = Map(
          "polling_interval_minutes" -> pollingIntervalMinutes.asInstanceOf[Object]
        )

        val markdown =
          s"""- [ ] Click to refresh now
             |- generated ${context.system.clock.now().toString.take(19)}
             |- pm02Compensated **$pm02Compensated** ($pm02Count)
             |- pm10Count $pm10Count
             |- CO2 **$rco2**
             |- tvocIndex $tvocIndex
             |
             |# Today
             |![[${dailyNoteName(context.system.clock.today())}]]
             |""".stripMargin

        val note = Note(markdown, frontmatter)

        noteRef.setTo(note) match {
          case Failure(exception) =>
            context.actorContext.log.error("Unexpected failure", exception)
          case Success(_) =>
        }

        Tinker.steadily

      case Subscribe(subscriber) =>
        if (subscribers.contains(subscriber)) {
          context.actorContext.log.debug(s"Duplicate subscriber: $subscriber")
          Tinker.steadily
        } else {
          context.actorContext.log.info(s"Adding new subscriber: $subscriber")
          behavior(pollingIntervalMinutes, subscribers + subscriber)
        }
    }
  }

  case class AirGradientSensorResult(noteId: NoteId, data: AirGradientSensorData)
}

private object AirGradientDailyMarkdown {
  def apply(items: List[AirGradientSensorData], clock: TinkerClock): String = {
    val (pm02Counts, pm10Counts, pm02Compensateds, rco2s, tvocIndexs) = split(items)

    // FIXME: .toInt hacks
    // FIXME: REMOVE .map(Math.min(_, 550) hacks (or add markdown mentioning it or SOMETHING)
    val pm02CountsSeries = IntSeries("pm02Counts", pm02Counts.map(_.toInt).map(Math.min(_, 200)))
    val pm10CountsSeries = DoubleSeries("pm10Counts", pm10Counts)
    val pm02CompensatedsSeries = IntSeries("pm02Compensateds", pm02Compensateds.map(_.toInt).map(Math.min(_, 200)))
    val rco2sSeries = IntSeries("rco2s-350", rco2s.map(_.toInt-350))
    val tvocIndexsSeries = DoubleSeries("tvocIndexs", tvocIndexs)

    val superimposed = ObsidianCharts.chart(List.fill(items.size)(""), List(
      pm02CountsSeries,
      pm10CountsSeries,
      pm02CompensatedsSeries,
      rco2sSeries,
      tvocIndexsSeries
    ))

    s"""# Superimposed
       |
       |$superimposed
       |
       |# pm10Counts
       |
       |${ObsidianCharts.chart(pm10CountsSeries)}
       |
       |# pm02Compensateds
       |
       |${ObsidianCharts.chart(pm02CompensatedsSeries)}
       |
       |# tvocIndexs
       |
       |${ObsidianCharts.chart(tvocIndexsSeries)}
       |
       |# rco2s-350
       |
       |${ObsidianCharts.chart(rco2sSeries)}
       |
       |# pm02Counts
       |
       |${ObsidianCharts.chart(pm02CountsSeries)}
       |""".stripMargin
  }

  def split(items: List[AirGradientSensorData]): (List[Double], List[Double], List[Double], List[Double], List[Double]) = {
    items.foldRight[(List[Double], List[Double], List[Double], List[Double], List[Double])]((Nil, Nil, Nil, Nil, Nil)) {
      case (AirGradientSensorData(pm02Count, pm10Count, pm02Compensated, rco2, tvocIndex), (pm02Counts, pm10Counts, pm02Compensateds, rco2s, tvocIndexs)) =>
        (pm02Count :: pm02Counts, pm10Count :: pm10Counts, pm02Compensated :: pm02Compensateds, rco2 :: rco2s, tvocIndex :: tvocIndexs)
    }
  }
}
