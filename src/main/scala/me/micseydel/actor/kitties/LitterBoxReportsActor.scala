package me.micseydel.actor.kitties

import me.micseydel.NoOp
import me.micseydel.actor.DailyNotesRouter
import me.micseydel.actor.FolderWatcherActor.Ping
import me.micseydel.actor.kitties.LitterCharts.{LitterReportForDay, LitterSummaryForDay}
import me.micseydel.app.MyCentralCast
import me.micseydel.dsl.*
import me.micseydel.dsl.Tinker.Ability

import java.time.ZonedDateTime


object LitterBoxReportsActor {
  sealed trait Message

  final case class ReceiveNotePing(ping: Ping) extends Message

  sealed trait EventCapture extends Message {
    def when: ZonedDateTime
  }

  case class LitterSiftedObservation(capture: LitterBoxesHelper.LitterSifted) extends EventCapture {
    def when: ZonedDateTime = capture.event.when
  }

  case class AddToInbox(string: String, when: ZonedDateTime) extends EventCapture

  // behavior

  def apply()(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = setup()


  private def setup()(implicit Tinker: EnhancedTinker[MyCentralCast]): Ability[Message] = Tinkerer(TinkerColor.CatBrown, "🗑️").setup { context =>
    implicit val c: TinkerContext[_] = context

    val DaysToActivateBack = 37

    val monthlyLitterGraphActor: SpiritRef[LitterSummaryForDay] = context.cast(MonthlyLitterGraphActor(), "MonthlyLitterGraphActor")
    val last30DaysLitterGraphActor: SpiritRef[LitterReportForDay] = context.cast(Last30DaysLitterGraphActor(), "Last30DaysLitterGraphActor")

    val dailyNotesAssistant: SpiritRef[DailyNotesRouter.Envelope[Message]] =
      context.cast(DailyNotesRouter(DailyLitterSummaryActor(_, _, _, monthlyLitterGraphActor, last30DaysLitterGraphActor), DaysToActivateBack), "DailyNotesRouter")

    Tinker.receiveMessage {
      case ec@LitterSiftedObservation(_) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(ec, ec.when)
        Tinker.steadily

      case ati@AddToInbox(_, when) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(ati, when)
        Tinker.steadily

      case np@ReceiveNotePing(NoOp) =>
        dailyNotesAssistant !! DailyNotesRouter.Envelope(np, context.system.clock.now())
        Tinker.steadily
    }
  }
}
