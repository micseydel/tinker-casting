package me.micseydel.actor

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import me.micseydel.actor.ActorNotesFolderWatcherActor.Ping
import me.micseydel.actor.AirGradientPollingActor.AlterPolling
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl._
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.AttentiveNoteMakingTinkerer
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.{DoubleSeries, IntSeries, Series}
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef
import spray.json._

import java.io.FileNotFoundException
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, DurationLong, FiniteDuration}
import scala.util.{Failure, Success}

object AirGradientActor {
  sealed trait Message

//  final case class Subscribe(subscriber: SpiritRef[RawSensorData]) extends Message
  final case class DoFetchNow() extends Message

  private case class ReceivePing(ping: Ping) extends Message

  private case class ReceiveAirGradientSensorData(data: AirGradientSensorData) extends Message

  private val BaseNoteName = "Air Gradient measurements"
  def apply(uri: String)(implicit Tinker: Tinker): Ability[Message] = AttentiveNoteMakingTinkerer[Message, ReceivePing](BaseNoteName, TinkerColor(145, 96, 220), "😮‍💨", ReceivePing) { (context, noteRef) =>
    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[AirGradientSensorData]]] = context.cast(DailyNotesRouter(
      BaseNoteName,
      "airgradient",
      AirGradientJsonFormat.airGradientSensorDataJsonFormat,
      DailyMarkdown.apply
    ), "DailyNotesRouter")

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

    val apiPoller: SpiritRef[AirGradientPollingActor.Message] = context.cast(AirGradientPollingActor(uri, initialPollingInterval, context.messageAdapter(ReceiveAirGradientSensorData)), "AirGradientPollingActor")

    behavior(initialPollingInterval.toMinutes
//      , Nil
    )(Tinker, noteRef,
      dailyNotesAssistant,
      apiPoller)
  }

  private def behavior(
                        pollingIntervalMinutes: Long
//                        , subscribers: List[SpiritRef[RawSensorData]]
                      )(implicit Tinker: Tinker, noteRef: NoteRef,
                        dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[DailyMarkdownFromPersistedMessagesActor.Message[AirGradientSensorData]]],
                        apiPoller: SpiritRef[AirGradientPollingActor.Message]): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val c: TinkerContext[_] = context
    message match {
//      case Subscribe(subscriber) =>
//        behavior(pollingIntervalMinutes, subscriber :: subscribers)

      case DoFetchNow() =>
        context.actorContext.log.info("Doing a fetch now")
        apiPoller !! AlterPolling(forceCheckNow = true, intervalUpdate = None)
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
                val refreshNow = note.markdown(3) match {
                  case 'x' => true
                  case ' ' => false
                  case other =>
                    context.actorContext.log.warn(s"Unexpected char $other for ${note.markdown}")
                    false
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

                if (refreshNow || updateIntervalTo.nonEmpty) {
                  val alteringPolling = AlterPolling(forceCheckNow = refreshNow, intervalUpdate = updateIntervalTo.map(_.minutes))
                  context.actorContext.log.debug(s"Altering polling! $alteringPolling")
                  apiPoller !! alteringPolling
                } else {
                  context.actorContext.log.info("Ignoring Frontmatter update - refreshNow was false and no valid update interval")
                }

                updateIntervalTo match {
                  case Some(updatedInterval) =>
                    behavior(updatedInterval
//                      , subscribers
                    )
                  case None => Tinker.steadily
                }
            }
        }

      case ReceiveAirGradientSensorData(data@AirGradientSensorData(pm02Count, pm10Count, pm02Compensated, rco2, tvocIndex)) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(
          DailyMarkdownFromPersistedMessagesActor.StoreAndRegenerateMarkdown(data),
          context.system.clock.now() // FIXME: hacky, prevents downstream from de-duping properly
        )

//        for (subscriber <- subscribers) {
//          subscriber !! data
//        }

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
             |![[$BaseNoteName (${context.system.clock.today()})]]
             |""".stripMargin

        val note = Note(markdown, frontmatter)

        noteRef.setTo(note) match {
          case Failure(exception) =>
            context.actorContext.log.error("Unexpected failure", exception)
          case Success(_) =>
        }

        Tinker.steadily
    }
  }

  private object DailyMarkdown {
    def apply(items: List[AirGradientSensorData], clock: TinkerClock): String = {
      val (pm02Counts, pm10Counts, pm02Compensateds, rco2s, tvocIndexs) = split(items)

      // FIXME: .toInt hacks
      val pm02CountsSeries = IntSeries("pm02Counts", pm02Counts.map(_.toInt))
      val pm10CountsSeries = DoubleSeries("pm10Counts", pm10Counts)
      val pm02CompensatedsSeries = IntSeries("pm02Compensateds", pm02Compensateds.map(_.toInt))
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
}


private object AirGradientPollingActor {
  sealed trait Message

  case object HeartBeat extends Message

  case class AlterPolling(forceCheckNow: Boolean, intervalUpdate: Option[FiniteDuration]) extends Message

  private case class ReceiveHttpResponse(httpResponse: HttpResponse) extends Message

  private case class ReceiveFailedHttpResponse(exception: Throwable) extends Message

  private case class ReceiveUnmarshalling(sensorData: AirGradientSensorData) extends Message

  def apply(uri: String, initialInterval: FiniteDuration, replyTo: SpiritRef[AirGradientSensorData])(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    context.actorContext.log.info(s"Making initial request then polling every $initialInterval")
    makeRequest(uri)
    val timeKeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
    timeKeeper !! TimeKeeper.RemindMeEvery(initialInterval, context.self, AirGradientPollingActor.HeartBeat, Some(AirGradientPollingActor.HeartBeat))

    behavior(uri, replyTo, timeKeeper)
  }

  private def behavior(uri: String, replyTo: SpiritRef[AirGradientSensorData], timeKeeper: SpiritRef[TimeKeeper.Message])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    implicit val s: ActorSystem[_] = context.system.actorSystem
    implicit val c: TinkerContext[Message] = context

    message match {
      case HeartBeat =>
        context.actorContext.log.info("Received heartbeat, making request...")
        makeRequest(uri)
        Tinker.steadily

      case inert@AlterPolling(false, None) =>
        context.actorContext.log.debug(s"Received inert AlterPolling request $inert")
        Tinker.steadily

      case a@AlterPolling(forceCheckNow, maybeDuration) =>
        context.actorContext.log.debug(s"AlterPolling $a")
        if (forceCheckNow) {
          context.actorContext.log.info("A check outside of the usual heartbeat has been requested, making API request now...")
          makeRequest(uri)
        }

        maybeDuration match {
          case Some(updatedInterval) =>
            context.actorContext.log.info(s"Updating polling interval to $updatedInterval")
            timeKeeper !! TimeKeeper.RemindMeEvery(updatedInterval, context.self, AirGradientPollingActor.HeartBeat, Some(AirGradientPollingActor.HeartBeat))
          case None =>
        }

        Tinker.steadily

      case ReceiveHttpResponse(httpResponse) =>
        context.actorContext.log.info("Received request response...")

        implicit val rawSensorJsonFormat: RootJsonFormat[AirGradientSensorData] = AirGradientJsonFormat.airGradientSensorDataJsonFormat
        val umarshal: Unmarshal[ResponseEntity] = Unmarshal(httpResponse.entity)
        val fut: Future[String] = umarshal.to[String]
        context.pipeToSelf(fut) {
          case Failure(exception) => ReceiveFailedHttpResponse(exception)
          case Success(models) =>
            try {
              ReceiveUnmarshalling(models.parseJson.convertTo[AirGradientSensorData])
            } catch {
              case e: DeserializationException =>
                ReceiveFailedHttpResponse(new RuntimeException(s"failed to parse: $models", e))
              case e: Throwable =>
                ReceiveFailedHttpResponse(new RuntimeException(s"something unexpected went wrong", e))
            }
        }

        Tinker.steadily

      case ReceiveFailedHttpResponse(exception) =>
        context.actorContext.log.warn(s"Failed to connect to local Air Gradient", exception)
        Tinker.steadily

      case ReceiveUnmarshalling(models) =>
        context.actorContext.log.info("Unmarshalling successful, replying and wrapping")
        replyTo !! models
        Tinker.steadily
    }
  }

  // util

  private def makeRequest(uri: String)(implicit context: TinkerContext[Message], actorSystem: ActorSystem[_]): Unit = {
    context.pipeToSelf(request(uri)) {
      case Failure(exception) => ReceiveFailedHttpResponse(exception)
      case Success(httpResponse) => ReceiveHttpResponse(httpResponse)
    }
  }

  private def request(uri: String)(implicit s: ActorSystem[_]): Future[HttpResponse] = {
    Http().singleRequest(HttpRequest(
      method = HttpMethods.GET,
      uri = uri,
      headers = List(
        headers.Accept(MediaTypes.`application/json`)
      )
    ))
  }
}

object AirGradientJsonFormat extends DefaultJsonProtocol {
  implicit val airGradientSensorDataJsonFormat: RootJsonFormat[AirGradientSensorData] = jsonFormat5(AirGradientSensorData)
}

/*
{
  "pm01": 0,
  "pm02": 0,
  "pm10": 0,
  "pm01Standard": 0,
  "pm02Standard": 0,
  "pm10Standard": 0,
  "pm003Count": 135.5,
  "pm005Count": 95.33,
  "pm01Count": 16.83,
  "pm02Count": 2.17,
  "pm50Count": 0.67,
  "pm10Count": 0.5,
  "atmp": 19.3,
  "atmpCompensated": 19.3,
  "rhum": 65.01,
  "rhumCompensated": 65.01,
  "pm02Compensated": 0,
  "rco2": 464,
  "tvocIndex": 88.25,
  "tvocRaw": 31276.58,
  "noxIndex": 1,
  "noxRaw": 16168.42,
  "boot": 5,
  "bootCount": 5,
  "wifi": -19,
  "ledMode": "pm",
  "serialno": "34b7dabd4bc4",
  "firmware": "3.1.11",
  "model": "I-9PSL"
}

 */
case class AirGradientSensorData(
//"pm01": 0,
//"pm02": 0,
//"pm10": 0,
//"pm01Standard": 0,
//"pm02Standard": 0,
//"pm10Standard": 0,
//"pm003Count": 135.5,
//"pm005Count": 95.33,
//"pm01Count": 16.83,
  pm02Count: Double,
//"pm50Count": 0.67,
  pm10Count: Double,
//"atmp": 19.3,
//"atmpCompensated": 19.3,
//"rhum": 65.01,
//"rhumCompensated": 65.01,
  pm02Compensated: Double,
  rco2: Double,
  tvocIndex: Double,
//"tvocRaw": 31276.58,
//"noxIndex": 1,
//"noxRaw": 16168.42,
//"boot": 5,
//"bootCount": 5,
//"wifi": -19,
//"ledMode": "pm",
//"serialno": "34b7dabd4bc4",
//"firmware": "3.1.11",
//"model": "I-9PSL"
                        ) {

//  def zonedDatetime: ZonedDateTime =
//    ZonedDateTime.parse(DateTime, Formatter)
//      .withZoneSameInstant(ZoneId.systemDefault())
}
