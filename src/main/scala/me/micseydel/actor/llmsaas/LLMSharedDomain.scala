package me.micseydel.actor.llmsaas

import spray.json.{DefaultJsonProtocol, JsonFormat}

object LLMSharedDomain {
  val DefaultMaxTokens = 16384 // FIXME 128000 was too big, OpenAI wanted 16384 for the default at time of this writing
}

// ── Shared domain types ───────────────────────────────────────────────────────

/** Minimal model descriptor, normalised across providers. */
final case class ModelInfo(id: String, ownedBy: String)

object SharedDomainJsonProtocol extends DefaultJsonProtocol {
  implicit val modelInfoJsonFormat: JsonFormat[ModelInfo] = jsonFormat2(ModelInfo)
  implicit val modelInfoListJsonFormat: JsonFormat[List[ModelInfo]] = listFormat(modelInfoJsonFormat)
}
