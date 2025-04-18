package me.micseydel.actor.kitties

import akka.event.slf4j.Logger
import me.micseydel.actor.kitties.kibble.KibbleManagerActor.KibbleRefill
import me.micseydel.actor.kitties.kibble.KibbleModel.{Circular1, Circular2, KibbleContainer, RectangularS}
import me.micseydel.actor.kitties.kibble.{KibbleManagerActor, KibbleManagerListenerActor, KibbleModel}
import me.micseydel.model.NotedTranscription
import me.micseydel.testsupport.TestHelpers
import me.micseydel.vault.NoteId
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{LocalDate, ZonedDateTime}
import scala.annotation.tailrec

class KibbleEventsIntegrationTestingSpec extends AnyWordSpec
  with MockFactory
  with BeforeAndAfterAll
  with Matchers {

  implicit val log: org.slf4j.Logger = Logger("kibblesimpletestlogger")

  private val RawEvents = List(
    ("2025-03-17T12:04", "I just refilled the primary circular dry food kibble container to 434 grams."),
    ("2025-03-17T12:05", "I just refilled the secondary circular dry food kibble container to 426 grams."),
    ("2025-03-17T12:05", "I just refilled the smaller rectangular dry food kibble container to 522 grams."), ("2025-03-18T07:48", "I just measured the small rectangular dry food kibble container at 426 grams."), ("2025-03-19T08:01", "I just discarded 29 grams of dry food kibble"), ("2025-03-19T08:02", "I just measured the small rectangular dry food kibble container at 311 grams."), ("2025-03-20T07:43", "I just discarded 21 grams of dry food kibble."), ("2025-03-20T07:43", "I just measured the small rectangular dry food kibble container at 196 grams."), ("2025-03-21T07:57", "I just discarded 37 grams of dry food kibble."), ("2025-03-21T07:57", "I just measured the primary circular dry food kibble container at 325 grams."), ("2025-03-22T07:19", "I just discarded 19 grams of dry food kibble."), ("2025-03-22T07:30", "I just measured the primary circular dry food kibble container at 167 grams."), ("2025-03-23T07:54", "I just measured the secondary dry food kibble container at 338 grams, the circular one."), ("2025-03-24T07:48", "I just measured the secondary circular dry food kibble container at 203 grams.")
  )

  "KibbleEventsIntegration" must {
    "..." in {
      val events: List[KibbleManagerActor.Message] = RawEvents
        .map(toSimpleNotedTranscription)
        .map(KibbleManagerListenerActor.lineToMeasurementEvent)
        .map(_.get)

      events match {
        case (first: KibbleRefill) :: (second: KibbleRefill) :: (third: KibbleRefill) :: theRest =>
          val (containersAfterDispensing, dispensed) = toTest(List(first, second, third), theRest)
          println(containersAfterDispensing)
          containersAfterDispensing match {
            case Containers(primCirc, secCirc, smallRec) =>
              println(s"Remaining: primCirc=${primCirc - Circular1.baselineWeight}, secCirc=${secCirc - Circular2.baselineWeight}, smallRec=${smallRec - RectangularS.baselineWeight} sum=${primCirc - Circular1.baselineWeight + secCirc - Circular2.baselineWeight + smallRec - RectangularS.baselineWeight}")
          }
          println(s"dispensed $dispensed ∑-> ${dispensed.totalDispensed} ~ ${containersAfterDispensing.total - Circular1.baselineWeight - Circular2.baselineWeight - RectangularS.baselineWeight}")

        case _ => ???
      }
    }
  }

  case class Containers(primCirc: Int, secCirc: Int, smallRec: Int) {
    override def toString: String = s"Containers(primCirc=$primCirc, secCirc=$secCirc, smallRec=$smallRec, total=$total)"

    def total: Int = primCirc + secCirc + smallRec
  }

  case class KibbleDispensingEntry(dispensed: Int = 0, discarded: Int = 0)
  case class KibbleDispensing(map: Map[LocalDate, KibbleDispensingEntry]) {
    def addDispensed(forDay: LocalDate, dispensed: Int): KibbleDispensing =
      this.copy(map.updatedWith(forDay) {
        case Some(KibbleDispensingEntry(previouslyDispensed, discarded)) =>
          Some(KibbleDispensingEntry(previouslyDispensed + dispensed, discarded))
        case None =>
          Some(KibbleDispensingEntry(dispensed = dispensed))
      })

    def addDiscard(forDay: LocalDate, discarded: Int): KibbleDispensing =
      this.copy(map.updatedWith(forDay) {
        case Some(KibbleDispensingEntry(dispensed, previouslyDiscarded)) =>
          Some(KibbleDispensingEntry(dispensed, previouslyDiscarded + discarded))
        case None =>
          Some(KibbleDispensingEntry(discarded = discarded))
      })

    def totalDispensed: Int = map.valuesIterator.map(_.dispensed).sum
  }

  def toTest(startingContents: List[KibbleRefill], events: List[KibbleManagerActor.Message]): (Containers, KibbleDispensing) = {
    val containers = startingContents match {
      // FIXME hardcoded
      case List(first, second, third) =>
        Containers(first.massGrams, second.massGrams, third.massGrams)

      case _ => ???
    }

    println(containers)

    @tailrec
    def r(currentContainers: Containers, remainingEvents: List[KibbleManagerActor.Event], accumulator: KibbleDispensing, lastUsedContainer: Option[KibbleContainer]): (Containers, KibbleDispensing) = {
      remainingEvents match {
        case Nil => (currentContainers, accumulator)
        case event :: eventsToRecursOn =>
          val (updatedContainers: Containers, updatedDispensing: KibbleDispensing, latestContainer: Option[KibbleContainer]) = event match {
            case KibbleManagerActor.KibbleDiscarded(massGrams, time, _) => (currentContainers, accumulator.addDiscard(time.toLocalDate, massGrams), lastUsedContainer)
            case measurement: KibbleManagerActor.KibbleContainerMeasurement =>
              val inferredRemainingFromPriorContainer = lastUsedContainer.filter(_ != measurement.container).map {
                case KibbleModel.Circular1 => currentContainers.primCirc - measurement.container.baselineWeight
                case KibbleModel.Circular2 => currentContainers.secCirc - measurement.container.baselineWeight
                case KibbleModel.RectangularS => currentContainers.smallRec - measurement.container.baselineWeight
                case KibbleModel.RectangularL => ???
              }.getOrElse(0)
              measurement.container match {
                case KibbleModel.Circular1 =>
                  val dispensed = currentContainers.primCirc - measurement.massGrams + inferredRemainingFromPriorContainer
                  val updatedAccumulator: KibbleDispensing = accumulator.addDispensed(event.time.toLocalDate, dispensed)
                  (currentContainers.copy(primCirc = measurement.massGrams), updatedAccumulator, Some(measurement.container))
                case KibbleModel.Circular2 =>
                  val dispensed = currentContainers.secCirc - measurement.massGrams + inferredRemainingFromPriorContainer
                  val updatedAccumulator: KibbleDispensing = accumulator.addDispensed(event.time.toLocalDate, dispensed)
                  (currentContainers.copy(secCirc = measurement.massGrams), updatedAccumulator, Some(measurement.container))
                case KibbleModel.RectangularS =>
                  val dispensed = currentContainers.smallRec - measurement.massGrams + inferredRemainingFromPriorContainer
                  val updatedAccumulator: KibbleDispensing = accumulator.addDispensed(event.time.toLocalDate, dispensed)
                  (currentContainers.copy(smallRec = measurement.massGrams), updatedAccumulator, Some(measurement.container))
                case KibbleModel.RectangularL => ???
              }
          }

          r(updatedContainers, eventsToRecursOn, updatedDispensing, latestContainer)
      }
    }

    val simplifiedEvents: List[KibbleManagerActor.Event] = events.map {
      case KibbleManagerActor.ReceiveVotes(votes) => ???
      case KibbleManagerActor.MaybeHeardKibbleMention(notedTranscription) => ???
      case event: KibbleManagerActor.Event => event
    }

    println(s"CANARY ${simplifiedEvents.map(_.time.toLocalDate).distinct}")

    r(containers, simplifiedEvents, KibbleDispensing(Map.empty), None)
  }

  //

  def toSimpleNotedTranscription(tuple: (String, String)): NotedTranscription = {
    val (rawStartTime: String, line: String) = tuple
    val time = ZonedDateTime.parse(rawStartTime + "-08:00[America/Los_Angeles]")
    val noteId = NoteId(KibbleManagerActorTestingSpec.generateFileName(time))
    TestHelpers.simpleNotedTranscription(line, time, noteId)
  }
}
