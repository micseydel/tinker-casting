package me.micseydel.actor.kitties

import akka.event.slf4j.Logger
import me.micseydel.actor.kitties.kibble.KibbleManagerActor.KibbleRefill
import me.micseydel.actor.kitties.kibble.{KibbleManagerActor, KibbleManagerListenerActor}
import me.micseydel.model.NotedTranscription
import me.micseydel.testsupport.TestHelpers
import me.micseydel.vault.NoteId
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.ZonedDateTime

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
      val t = RawEvents
        .map(toSimpleNotedTranscription)
        .map(KibbleManagerListenerActor.lineToMeasurementEvent)
        .map(_.get)

      t match {
        case (first: KibbleRefill) :: (second: KibbleRefill) :: (third: KibbleRefill) :: theRest =>
        case _ => ???
      }

      println(t)
    }
  }

  def toTest(): Unit = {

  }

  //

  def toSimpleNotedTranscription(tuple: (String, String)): NotedTranscription = {
    val (rawStartTime: String, line: String) = tuple
    val time = ZonedDateTime.parse(rawStartTime + "-08:00[America/Los_Angeles]")
    val noteId = NoteId(KibbleManagerActorTestingSpec.generateFileName(time))
    TestHelpers.simpleNotedTranscription(line, time, noteId)
  }
}
