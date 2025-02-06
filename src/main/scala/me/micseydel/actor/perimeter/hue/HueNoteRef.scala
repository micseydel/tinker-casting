package me.micseydel.actor.perimeter.hue

import me.micseydel.NoOp
import me.micseydel.actor.perimeter.HueControl
import me.micseydel.vault.persistence.NoteRef

import scala.util.{Failure, Success}

// FIXME: ideally the underlying reads would return Validated objects, to report parsing and other issues
private[perimeter] class HueNoteRef(noteRef: NoteRef) {
  private val commandsMap: Map[String, HueControl.Command] = Map(
    "Do a light show 🌈" -> HueControl.DoALightShow(),
    "Flash the lights 💡" -> HueControl.FlashTheLights()
  )

  //

  def setToDefault(): NoOp.type = {
    val default = commandsMap.keys.mkString("- [ ] ", "\n- [ ] ", "\n")
    setMarkdown(default)
  }

  def checkForCheckbox(): Option[HueControl.Command] = {
    noteRef.readMarkdown().map { markdown =>
      markdown.split("\n").toList.collectFirst { line =>
        line(3) match {
          case 'x' =>
            commandsMap.get(line.drop(6))
        }
      }.flatten
    } match {
      case Failure(exception) => throw exception
      case Success(maybeCommand) => maybeCommand
    }
  }

  //

  private def setMarkdown(markdown: String): NoOp.type = {
    noteRef.setMarkdown(markdown) match {
      case Failure(exception) => throw exception
      case Success(noOp) => noOp
    }
  }
}
