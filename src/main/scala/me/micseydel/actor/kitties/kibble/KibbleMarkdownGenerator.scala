//package me.micseydel.actor.kitties.kibble
//
//import me.micseydel.actor.kitties.kibble.KibbleManagerActor._
//import me.micseydel.actor.kitties.kibble.KibbleModel._
//import me.micseydel.util.MarkdownUtil
//import org.slf4j.Logger
//
//import java.time.format.DateTimeFormatter
//import java.time.{LocalDate, ZonedDateTime}
//
//object KibbleMarkdownGenerator {
//  def apply(allEvents: List[Message])(implicit log: Logger): String = {
//    val batchedEvents = allEvents
//      .groupBy(_.time.toLocalDate)
//      .view
//      .mapValues(_.sortBy(_.time))
//      .toList
//      .sortBy(_._1)
//
//    val (latestMeasurements, days, _) = batchedEvents.foldLeft((LatestMeasurements(None, None, None, None), Nil: List[WithinDayState], 0)) { case ((latestMeasurementsSoFar, integratedDays, carryOver), (day, events)) =>
//      log.debug(s"New day $day, about to integrate ${events.size} events $events")
//      val (todaysState, updatedMeasures) = events.foldLeft((WithinDayState(day, consumed = carryOver), latestMeasurementsSoFar)) { case ((withinDayState, priorBaseline), event) =>
//        withinDayState.integrate(event, priorBaseline)
//      }
//
//      (updatedMeasures, todaysState :: integratedDays, todaysState.carryOver)
//    }
//
//    val last7DaysConsumption = days.dropRight(1).map(day => MarkdownUtil.listLineWithTimestampForDay(day.day.minusDays(1), s"${day.netConsumed}")).mkString("\n")
//
//    val eventLines = allEvents.map { event =>
//      val lineText = event match {
//        case KibbleRefill(container, massGrams, _, _) => s"refilled $container with ${massGrams}g"
//        case RemainingKibbleMeasure(container, massGrams, _, _) => s"remaining in $container is ${massGrams}g"
//        case KibbleDiscarded(massGrams, _, _) => s"discarded ${massGrams}g"
//      }
//
//      MarkdownUtil.listLineWithTimestampAndRef(event.time, lineText, event.noteId, dateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME)
//    }.mkString("\n")
//
//    s"""# Current state
//       |
//       |${latestMeasurements.toMarkdownLines}
//       |
//       |# Last 7 days consumption
//       |
//       |$last7DaysConsumption
//       |
//       |# Events
//       |
//       |$eventLines""".stripMargin
//  }
//}
//
//
//case class WithinDayState(day: LocalDate,
//                          discarded: Int = 0, consumed: Int = 0,
//                          carryOver: Int = 0) {
//  def pretty: String = {
//    this.toString
//  }
//
//  /*
//  - \[2024-11-08\] 392 - 273 -23 = 119
//  - \[2024-11-07\] 502 - 392 = 110
//  - \[2024-11-06\] (289 - 189) + (558 - 502) -32 = 124
//  - \[2024-11-05\] 430 - 289 -24 = 117
//  - \[2024-11-04\] 545 - 430 = 115
//   */
//  def integrate(event: Message, latestMeasurements: LatestMeasurements)(implicit log: Logger): (WithinDayState, LatestMeasurements) = {
//    event match {
//      case KibbleDiscarded(newlyDiscarded, _, _) =>
//        (this.copy(discarded = discarded + newlyDiscarded), latestMeasurements)
//
//      case refill@KibbleRefill(refilledContainer, _, _, _) =>
//        latestMeasurements.getLatestMeasurementForContainer(refilledContainer) match {
//          case None =>
//            (this, latestMeasurements.updateContainer(refill))
//
//          case Some(lastMeasureBeforeRefill) =>
//            val updated = this.copy(
//              carryOver = carryOver + lastMeasureBeforeRefill.massGrams - lastMeasureBeforeRefill.container.baselineWeight
//            )
//            (updated, latestMeasurements.updateContainer(refill))
//        }
//
//      case measure@RemainingKibbleMeasure(container, remainingMassGramsToday, _, _) =>
//        (latestMeasurements.veryLatest, latestMeasurements.getLatestMeasurementForContainer(container)) match {
//          case (_, None) =>
//            (this, latestMeasurements.updateContainer(measure))
//
//          case (Some(latestMeasure), Some(mostRecentMeasureForContainer)) =>
//            // FIXME: when we switch containers, we infer the prior one is empty but need to treat that as a measurement
//            if (latestMeasure.container != container) {
//              val newlyConsumed = (mostRecentMeasureForContainer.massGrams - remainingMassGramsToday) + (
//                if (latestMeasure.isInstanceOf[RemainingKibbleMeasure] && mostRecentMeasureForContainer.isInstanceOf[KibbleRefill]) {
//                  latestMeasure.massGrams - latestMeasure.container.baselineWeight
//                } else {
//                  0
//                })
//              val updated = this.copy(consumed = consumed + newlyConsumed)
//              (updated, latestMeasurements.updateContainer(measure))
//            } else {
//              val consumedToday = mostRecentMeasureForContainer.massGrams - remainingMassGramsToday
//              val updated = this.copy(consumed = consumed + consumedToday)
//              (updated, latestMeasurements.updateContainer(measure))
//            }
//
//          case (None, Some(found)) =>
//            throw new RuntimeException(s"BUG latestMeasurements.veryLatest was ${latestMeasurements.veryLatest} but latestMeasurements.getLatestMeasurementForContainer($container) produced $found")
//        }
//    }
//  }
//
//  def netConsumed: Int = consumed - discarded
//}
//
//case class LatestMeasurements(
//                               latestCircular1: Option[KibbleContainerMeasurement],
//                               latestCircular2: Option[KibbleContainerMeasurement],
//                               latestRectangularS: Option[KibbleContainerMeasurement],
//                               latestRectangularL: Option[KibbleContainerMeasurement]
//                             ) {
//  def veryLatest: Option[KibbleContainerMeasurement] =
//    List(latestCircular1, latestCircular2, latestRectangularS, latestRectangularL)
//      .flatten
//      .maxByOption(_.time)
//
//  def updateContainer(measurement: KibbleContainerMeasurement): LatestMeasurements = {
//    measurement.container match {
//      case Circular1 => this.copy(latestCircular1 = Some(measurement))
//      case Circular2 => this.copy(latestCircular2 = Some(measurement))
//      case RectangularS => this.copy(latestRectangularS = Some(measurement))
//      case RectangularL => this.copy(latestRectangularL = Some(measurement))
//    }
//  }
//
//  def getLatestMeasurementForContainer(container: KibbleContainer): Option[KibbleContainerMeasurement] = {
//    container match {
//      case Circular1 => latestCircular1
//      case Circular2 => latestCircular2
//      case RectangularS => latestRectangularS
//      case RectangularL => latestRectangularL
//    }
//  }
//
//  def toMarkdownLines: String = {
//    s"""- latestCircular1 ${latestCircular1.map(_.massGrams).getOrElse("?")}
//       |- latestCircular2 ${latestCircular2.map(_.massGrams).getOrElse("?")}
//       |- latestRectangularS ${latestRectangularS.map(_.massGrams).getOrElse("?")}
//       |- latestRectangularL ${latestRectangularL.map(_.massGrams).getOrElse("?")}
//       |- (generated: ${ZonedDateTime.now()})
//       |""".stripMargin
//  }
//}
