package me.micseydel.actor

import me.micseydel.actor.SleepReportActor.SleepReportState
import me.micseydel.actor.perimeter.fitbit.FitbitActor
import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepReport
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.TinkerColor.rgb
import me.micseydel.dsl.cast.TimeKeeper
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerContext}
import me.micseydel.prototyping.ObsidianCharts
import me.micseydel.prototyping.ObsidianCharts.Series
import me.micseydel.util.TimeUtil
import me.micseydel.vault.persistence.{NoteRef, TypedJsonRef}
import spray.json._

import java.io.FileNotFoundException
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import scala.util.{Failure, Success}

object SleepReportActor {
  sealed trait Message

  private final case class ReceiveSleepReport(report: SleepReport) extends Message

  private final case class HeartBeat() extends Message

  // behaviors

  def apply(fitbitActor: SpiritRef[FitbitActor.Message], subscriber: SpiritRef[SleepReport])(implicit Tinker: Tinker): Ability[Message] =
    NoteMakingTinkerer("Sleep Report", rgb(20, 20, 20), "😴") { case (context, noteRef) =>
      implicit val c: TinkerContext[Message] = context

      //        val interval = 4.hours
      //        context.log.info(s"Initializing sleep report with refresh interval $interval, but doing a a pull right now too")

      val timekeeper: SpiritRef[TimeKeeper.Message] = context.castTimeKeeper()
      //        timekeeper !! TimeKeeper.RemindMeEvery(interval, context.self, HeartBeat(), Some(HeartBeat.getClass))
      val hours = List(3, 7, 13, 16, 19)
      context.actorContext.log.info(s"Just set reminders for 10 minutes after each of these hours after midnight: $hours")
      hours.foreach { hour =>
        timekeeper !! TimeKeeper.RemindMeDailyAt(hour, 10, context.self, HeartBeat(), None)
      }

      context.actorContext.log.debug(s"Uncomment RequestSleep(context.messageAdapter(ReceiveSleepReport) line to fetch sleep data at startup")

      // uncomment this to fetch today's Fitbit sleep report on startup (instead of just at scheduled times)
      //                fitbitActor !! FitbitActor.RequestSleep(context.messageAdapter(ReceiveSleepReport), context.system.clock.today())
//      fitbitActor !! FitbitActor.RequestSleep(context.messageAdapter(ReceiveSleepReport), context.system.clock.today().minusDays(1))

      // uncomment to refresh Markdown on startup (e.g. if you're tinkering)
      //        jsonRef.read() match {
      //          case Failure(_: FileNotFoundException) =>
      //            context.actorContext.log.debug(s"File $jsonRef not found, deferring Markdown creation")
      //          case Failure(exception) => throw exception
      //
      //          case Success(reportState) =>
      //            context.actorContext.log.info("Regenerating markdown")
      //            noteRef.setMarkdown(SleepReportMarkdown(reportState, context.system.clock.today()))
      //        }

      Tinker.withTypedJson("sleep_report", StateJsonProtocol.stateJsonFormat) { jsonRef =>
        behavior(fitbitActor, subscriber, jsonRef, noteRef)
      }
    }

  private def behavior(fitbitActor: SpiritRef[FitbitActor.Message], subscriber: SpiritRef[SleepReport], jsonRef: TypedJsonRef[SleepReportState], noteRef: NoteRef)(implicit Tinker: Tinker): Ability[Message] = Tinker.setup { context =>
    implicit val c: TinkerContext[_] = context
    Tinker.receiveMessage {
      case ReceiveSleepReport(receivedReport) =>
        receivedReport.sleep.headOption match {
          case Some(sleepEntry) =>
            val receivedForDay = sleepEntry.dateOfSleep
            context.actorContext.log.info(s"Received sleep report for day $receivedForDay, updating JSON and notifying subscriber ${subscriber.path}")

            val latest = jsonRef.read() match {
              case Failure(_: FileNotFoundException) =>
                context.actorContext.log.debug(s"File $jsonRef not found, using empty state")
                SleepReportState.empty
              case Success(reportState) =>
                reportState.getDay(receivedForDay) match {
                  case None =>
                  case Some(existingReport) =>
                    val existingEndTimes = existingReport.sleep.map(_.endTime)
                    val receivedEndTimes = receivedReport.sleep.map(_.endTime)
                    val msg = s"Replacing existingEndTimes $existingEndTimes with receivedEndTimes $receivedEndTimes"
                    context.actorContext.log.debug(msg)
                }
                reportState
              case Failure(exception) => throw exception
            }

            val updated = latest.setDay(receivedForDay, receivedReport)
            jsonRef.set(updated)

            subscriber !! receivedReport

            context.actorContext.log.info("Updated JSON state, setting Markdown now...")
            noteRef.setMarkdown(SleepReportMarkdown(updated, context.system.clock.today()))
          case None =>
            context.actorContext.log.debug(s"receivedReport.sleep was empty ${receivedReport.sleep} ")
        }

        Tinker.steadily

      case HeartBeat() =>
        implicit val c: TinkerContext[Message] = context
        val today = context.system.clock.today()
        context.actorContext.log.info(s"Received heartbeat, fetching $today")
        fitbitActor !! FitbitActor.RequestSleep(context.messageAdapter(ReceiveSleepReport), today)
        Tinker.steadily
    }
  }

  //

  case class SleepReportState(rawByDay: Map[String, SleepReport]) {
    def setDay(day: String, report: SleepReport): SleepReportState = {
      SleepReportState(rawByDay.updated(day, report))
    }

    def getDay(day: LocalDate): Option[SleepReport] = rawByDay.get(TimeUtil.localDateTimeToISO8601Date(day))

    /**
     * ISO date
     */
    def getDay(day: String): Option[SleepReport] = rawByDay.get(day)
  }

  object SleepReportState {
    def empty: SleepReportState = SleepReportState(Map())
  }

  object StateJsonProtocol extends DefaultJsonProtocol {

    import me.micseydel.actor.perimeter.fitbit.FitbitModel.SleepJsonProtocol.sleepReportFormat

    implicit val stateJsonFormat: RootJsonFormat[SleepReportState] = jsonFormat1(SleepReportState.apply)
  }
}

object SleepReportMarkdown {
  def apply(reports: SleepReportState, today: LocalDate): String = {

    def lastNdays(n: Int): List[Option[SleepReport]] = (0 until n).toList.map { daysAgo =>
      reports.getDay(today.minusDays(daysAgo))
    }

    val charts = if (reports.rawByDay.size >= 14) {
      val last7Days: List[Int] = lastNdays(7).map(_.map(_.summary.totalMinutesAsleep).getOrElse(0))
      val last14Days: List[Int] = lastNdays(14).map(_.map(_.summary.totalMinutesAsleep).getOrElse(0))
      val last30Days: List[Int] = lastNdays(30).map(_.map(_.summary.totalMinutesAsleep).getOrElse(0))
      val last37Days: List[Int] = lastNdays(37).map(_.map(_.summary.totalMinutesAsleep).getOrElse(0))

      def runningAverage(elements: List[Int], lookback: Int): List[Int] = {
        elements.indices.map { i =>
          val window = elements.slice(Math.max(0, i - lookback + 1), i + 1)
          window.sum / window.length
        }.toList
      }

      val last30daysLabels: List[String] = (0 until 30).map { daysAgo =>
        if (daysAgo == 0) {
          "Today"
        } else if (daysAgo == 29) {
          "A month ago"
        } else if (daysAgo % 2 == 1) {
          ""
        } else {
          ZonedDateTime.now().minusDays(daysAgo).format(DateTimeFormatter.ofPattern("M-d"))
        }
      }.toList.reverse

      val last7DaysChart = ObsidianCharts.chart(
        List("6 days ago", "-5", "-4", "-3", "-2", "-1", "Today"),
        List(
          Series("Suggested minimum", List.fill(7)(360)),
          Series("Total minutes of sleep", last7Days.reverse)
        )
      )

      val last14DaysChart = ObsidianCharts.chart(
        List("A couple weeks ago", "-12", "-11", "-10", "-9", "-8", "A week ago", "-6", "-5", "-4", "-3", "-2", "-1", "Today"),
        List(
          Series("Suggested minimum", List.fill(14)(360)),
          Series("Total minutes of sleep", last14Days.reverse)
        )
      )

      val last30DaysChart = ObsidianCharts.chart(
        last30daysLabels,
        List(
          Series("Suggested minimum", List.fill(30)(360)),
          Series("Total minutes of sleep", last30Days.reverse),
          Series("Average sleep last 7 days", runningAverage(last37Days, 7).reverse)
        )
      )

      s"""## Last 7 days
         |
         |$last7DaysChart
         |
         |## Last 14 days
         |
         |$last14DaysChart
         |
         |## Last 30 days
         |
         |$last30DaysChart
         |""".stripMargin
    } else {
      ""
    }

    val last7Days: List[SleepReport] = lastNdays(7).flatten

    lastNdays(3) match {
      case List(Some(todaysReport), Some(yesterdaysReport), Some(dayBeforeYesterdaysPost)) =>
        val recentDays = List(todaysReport, yesterdaysReport, dayBeforeYesterdaysPost)
          .map(_.summary.totalMinutesAsleep)
        val totalSleptMinutesLast3Days = recentDays.sum
        val averageSleptMinutesLast3Days = totalSleptMinutesLast3Days / 3

        val averageLast7Days = Some({
          val averageMinutesLast7Days = TimeUtil.minuteToHoursAndMinutes(last7Days.map(_.summary.totalMinutesAsleep).sum / 7)
          s"- Average last 7 days: **$averageMinutesLast7Days**"
        })
          .filter(_ => last7Days.size == 7)
          .getOrElse("")

        s"""- Last night's total: **${TimeUtil.minuteToHoursAndMinutes(todaysReport.summary.totalMinutesAsleep)}**
           |- Average last 3 days: **${TimeUtil.minuteToHoursAndMinutes(averageSleptMinutesLast3Days)}**
           |$averageLast7Days
           |- This report was generated ${ZonedDateTime.now()}
           |
           |# Charts
           |
           |$charts
           |""".stripMargin

      case other =>
        s"Unexpected: $other"
    }
  }
}
