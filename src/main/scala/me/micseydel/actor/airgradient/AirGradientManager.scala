package me.micseydel.actor.airgradient

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{SpiritRef, Tinker, TinkerColor}
import me.micseydel.vault.persistence.NoteRef
import net.jcazevedo.moultingyaml.{PimpedString, *}

import scala.util.{Failure, Success, Try}

object AirGradientManager {
  sealed trait Message

  val BaseNoteName = "Air Gradient Manager"

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer[Message](BaseNoteName, TinkerColor(145, 96, 220), "🕸️", Some("_actor_notes")) { (context, noteRef) =>

    noteRef.getDevices() match {
      case Failure(exception) =>
        context.actorContext.log.error(s"Failed to get devices from [[$BaseNoteName]]", exception)
        Tinker.unhandled

      case Success(devices) =>
        devices match {
          case None =>
            context.actorContext.log.warn(s"Found no AirGradient config in [[$BaseNoteName]]")
            Tinker.unhandled

          case Some(Config(devices)) =>
            val deviceActors = devices.map {
              case Device(host, nickname) =>
                // FIXME: this is fragile, what to do instead?
                // e.g. airgradient_34b7dabd4bc4.local -> 34b7dabd4bc4
                val withoutPrefix = if (host.startsWith("airgradient_")) {
                  host.drop("airgradient_".length)
                } else {
                  host
                }

                val withoutLocal = if (withoutPrefix.endsWith(".local")) {
                  withoutPrefix.dropRight(".local".length)
                } else {
                  withoutPrefix
                }

                val airGradientApiActor = context.castAnonymous(AirGradientApiActor(host))

                val (noteName, ability) = AirGradientActor(airGradientApiActor, serial = withoutLocal, nickname)
                noteName -> context.castAnonymous(ability)
            }

            val noteNames = deviceActors.map(_._1)
            noteRef.setMarkdown(noteNames.map(n => s"- [[$n]]").mkString("\n"))

            val actors = deviceActors.map(_._2)
            behavior(actors)
        }
    }
  }

  private def behavior(deviceActors: List[SpiritRef[AirGradientActor.Message]])(implicit Tinker: Tinker): Ability[Message] = Tinker.receive { (context, message) =>
    context.actorContext.log.warn(s"Did not expect any message! deviceActors: $deviceActors")
    Tinker.steadily
  }

  //

  private case class Device(server: String, nickname: Option[String])

  // FIXME: ideally a nonempty list
  private case class Config(devices: List[Device])

  private implicit class RichNoteRef(val noteRef: NoteRef) extends AnyVal {
    import ConfigSerialization.configYamlFormat
    def getDevices(): Try[Option[Config]] = {
      noteRef.readNote()
        .flatMap(note => Try(note.maybeFrontmatter.map(_.parseYaml.convertTo[Config])))
    }
  }

  private object ConfigSerialization extends DefaultYamlProtocol {
    implicit val deviceYamlFormat: YamlFormat[Device] = yamlFormat2(Device)
    implicit val configYamlFormat: YamlFormat[Config] = yamlFormat1(Config)
  }
}
