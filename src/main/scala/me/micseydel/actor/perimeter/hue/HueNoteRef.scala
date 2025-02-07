package me.micseydel.actor.perimeter.hue

import me.micseydel.actor.perimeter.HueControl
import me.micseydel.actor.perimeter.HueControl.{SetAllBrightness, SetAllLights}
import me.micseydel.actor.perimeter.hue.HueNoteRef.{BrightnessKey, ColorKey, CommandsMap, Properties}
import me.micseydel.model.{Color, LightState}
import me.micseydel.vault.Note
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

// FIXME: ideally the underlying reads would return Validated objects, to report parsing and other issues
private[perimeter] class HueNoteRef(noteRef: NoteRef) {
  def setToDefault(): Note = {
    val frontmatter: Map[String, Object] = Map(
      BrightnessKey -> -1.asInstanceOf[Object],
      ColorKey -> "?".asInstanceOf[Object]
    )

    val default = Note(
      CommandsMap.keys.mkString("- [ ] ", "\n- [ ] ", "\n"),
      frontmatter
    )
    setNote(default)
  }

  def checkForCommand(): Option[HueControl.Command] = {
    noteRef.readNote() match {
      case Failure(exception) => throw exception
      case Success(note@Note(markdown, _)) =>
        note.yamlFrontMatter.toOption.map(Properties.fromMap).collect {
          case Properties(Some(brightness), None) =>
            SetAllBrightness(brightness)
          case Properties(None, Some(color)) =>
            SetAllLights(color)
          case Properties(Some(brightness), Some(color)) =>
            SetAllLights(color.copy(bri = brightness))
        }.orElse {
          markdown.split("\n").toList.collectFirst { case line if line.length >= 6 && line.charAt(3) == 'x' =>
            CommandsMap.get(line.drop(6))
          }.flatten
        }
    }
  }

  //

  private def setNote(note: Note): Note = {
    noteRef.setTo(note) match {
      case Failure(exception) => throw exception
      case Success(note) => note
    }
  }
}

object HueNoteRef {
  private val BrightnessKey = "brightness"
  private val ColorKey = "color"

  private val CommandsMap: Map[String, HueControl.Command] = Map(
    "Do a light show 🌈" -> HueControl.DoALightShow(),
    "Flash the lights 💡" -> HueControl.FlashTheLights()
  )

  case class Properties(brightness: Option[Int], color: Option[LightState])

  object Properties {
    def fromMap(map: Map[String, Any]): Properties = {
      val brightness = map.get(BrightnessKey).collect {
        case int: Int if int >= 0 && int <= 100 => int
      }

      val color = map.get(ColorKey).map(_.toString).flatMap(Color.unapply)

      Properties(brightness, color)
    }
  }
}
