package me.micseydel.testsupport

import me.micseydel.model.{LargeModel, NotedTranscription, TranscriptionCapture, WhisperResult, WhisperResultContent, WhisperResultMetadata, WhisperSegment}
import me.micseydel.vault.NoteId
import me.micseydel.vault.persistence.NoteRef

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success}

object TestHelpers {
  def log(string: String): Unit = {
    val now = ZonedDateTime.now().toString.substring(11, 26)
    println(s"[$now] $string")
  }

  def compressedNewlines(s: String): String = {
    s.replace("\n", "\\n")
  }

  def simpleNotedTranscription(text: String): NotedTranscription = {
    simpleNotedTranscription(text, ZonedDateTime.now(), NoteId(UUID.randomUUID().toString))
  }

  def simpleNotedTranscription(text: String, time: ZonedDateTime, noteId: NoteId): NotedTranscription = {
    val whisperResult = WhisperResult(WhisperResultContent(text, List(WhisperSegment(0, text))), WhisperResultMetadata(LargeModel, "performedOn", "vaultPath", -1))

    NotedTranscription(TranscriptionCapture(whisperResult, time), noteId)
  }
}
