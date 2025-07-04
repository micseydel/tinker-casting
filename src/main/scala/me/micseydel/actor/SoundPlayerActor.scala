package me.micseydel.actor

import me.micseydel.dsl.{Tinker, TinkerColor}
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer

import javax.sound.sampled.AudioSystem

object SoundPlayerActor {
  sealed trait Message

  final case class PlaySound(path: String) extends Message

  def apply()(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Sound Player", TinkerColor.random(), "🎙️", Some("_actor_notes")) { (context, noteRef) =>

    val mixers = AudioSystem.getMixerInfo
    noteRef.setMarkdown(mixers.map("- " + _.getName).mkString("\n"))

    Tinker.receiveMessage {
      case PlaySound(path) =>
        context.actorContext.log.info(s"Playing sound at $path")
        playSoundfile(path)
        Tinker.steadily
    }
  }

  private def playSoundfile(f: String): Unit = {
    val audioInputStream = AudioSystem.getAudioInputStream(new java.io.File(f))
    val clip = AudioSystem.getClip
    clip.open(audioInputStream)
    //    val floatGainControl = clip.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
    //    floatGainControl.setValue(gainControl)  //reduce volume by x decibels (like -10f or -20f)
    clip.start() // doesn't block!
  }
}
