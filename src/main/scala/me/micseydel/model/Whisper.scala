package me.micseydel.model

import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat}


sealed trait WhisperModel
case object BaseModel extends WhisperModel
case object LargeModel extends WhisperModel

case class WhisperResultMetadata(
                                  model: WhisperModel,
                                  performedOn: String,
                                // FIXME: pathINVault? also the flask server seems to provide the FULL PATH ugh
                                  vaultPath: String,
                                  perfCounterElapsed: Double
                                )

case class WhisperResultContent(
                                 text: String,
                                 segments: List[WhisperSegment]
                               )

case class WhisperSegment(
                         start: Float,
                         text: String
                         // id, ...
                         )

case class WhisperResult(
                          whisperResultContent: WhisperResultContent,
                          whisperResultMetadata: WhisperResultMetadata
                        )

object WhisperResultJsonProtocol extends DefaultJsonProtocol {
  implicit object WhisperModelJsonFormat extends RootJsonFormat[WhisperModel] {
    def write(wm: WhisperModel): JsString = JsString(wm.toString)

    def read(value: JsValue): WhisperModel = value match {
      case JsString(s) => s match {
        case "BaseModel" | "base_model" | "base" => BaseModel
        case "LargeModel" | "large_model" | "large" => LargeModel
        case _ => throw DeserializationException(s"Expected BaseModel (or camel case or 'base') or LargeModel but got $s")
      }
      case _ => throw DeserializationException("Expected a string")
    }
  }

  implicit val whisperSegmentFormat: RootJsonFormat[WhisperSegment] = jsonFormat2(WhisperSegment)
  implicit val whisperResultContentFormat: RootJsonFormat[WhisperResultContent] = jsonFormat2(WhisperResultContent)
  implicit val whisperResultMetadataFormat: RootJsonFormat[WhisperResultMetadata] = jsonFormat4(WhisperResultMetadata)

  implicit val whisperResultFormat: RootJsonFormat[WhisperResult] = jsonFormat2(WhisperResult)
}
