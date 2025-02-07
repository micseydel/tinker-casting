package me.micseydel.dsl.tinkerer

import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.{Tinker, TinkerColor, TinkerContext, Tinkerer}
import me.micseydel.vault.persistence.NoteRef

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object NoteMakingTinkerer {
  def apply[M](noteName: String, color: TinkerColor, emoji: String, subdirectory: Option[String] = None)(ability: (TinkerContext[M], NoteRef) => Ability[M])(implicit Tinker: Tinker): Ability[M] = {
    // FIXME: vault name should be configurable
    val href = noteNameToObsidianUrl(noteName)
    Tinkerer[M](color, emoji, Some(href)).withNote(noteName, subdirectory)(ability)
  }

  private def encode(raw: String): String = {
    // FIXME https://chatgpt.com/c/67a64145-bc64-800e-bf91-c807c196fe5b
    //   this is awful, replace it with something more principled
    URLEncoder.encode(raw, StandardCharsets.UTF_8.toString).replace("+", "%20")
  }

  def noteNameToObsidianUrl(noteName: String): String = "obsidian://open?vault=deliberate_knowledge_accretion&file=" + encode(noteName)
}
