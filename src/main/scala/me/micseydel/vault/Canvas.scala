package me.micseydel.vault

import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import spray.json._

object Canvas {

}

// Define the case classes for the Canvas model
case class CanvasData(nodes: List[AllCanvasNodeData], edges: List[CanvasEdgeData])

sealed trait AllCanvasNodeData extends Product with Serializable {
  def id: String
}

case class CanvasFileData(id: String, x: Int, y: Int, width: Int, height: Int, color: Option[String],
                          `type`: String, file: String, subpath: Option[String]) extends AllCanvasNodeData

case class CanvasTextData(id: String, x: Int, y: Int, width: Int, height: Int, color: Option[String],
                          `type`: String, text: String) extends AllCanvasNodeData

case class CanvasLinkData(id: String, x: Int, y: Int, width: Int, height: Int, color: Option[String],
                          `type`: String, url: String) extends AllCanvasNodeData

case class CanvasGroupData(id: String, x: Int, y: Int, width: Int, height: Int, color: Option[String],
                           `type`: String, label: Option[String], background: Option[String],
                           backgroundStyle: Option[String]) extends AllCanvasNodeData

case class CanvasEdgeData(id: String, fromNode: String, fromSide: String, fromEnd: Option[String],
                          toNode: String, toSide: String, toEnd: Option[String], color: Option[String], label: Option[String])

// Define Spray JSON formats for serialization
object CanvasJsonFormats extends DefaultJsonProtocol {
  implicit val canvasEdgeDataFormat: RootJsonFormat[CanvasEdgeData] = jsonFormat9(CanvasEdgeData)
  implicit val canvasFileDataFormat: RootJsonFormat[CanvasFileData] = jsonFormat9(CanvasFileData)
  implicit val canvasTextDataFormat: RootJsonFormat[CanvasTextData] = jsonFormat8(CanvasTextData)
  implicit val canvasLinkDataFormat: RootJsonFormat[CanvasLinkData] = jsonFormat8(CanvasLinkData)
  implicit val canvasGroupDataFormat: RootJsonFormat[CanvasGroupData] = jsonFormat10(CanvasGroupData)

  implicit object AllCanvasNodeDataFormat extends RootJsonFormat[AllCanvasNodeData] {
    def write(data: AllCanvasNodeData): JsValue = data match {
      case fileData: CanvasFileData => fileData.toJson
      case textData: CanvasTextData => textData.toJson
      case linkData: CanvasLinkData => linkData.toJson
      case groupData: CanvasGroupData => groupData.toJson
    }

    def read(value: JsValue): AllCanvasNodeData = value.asJsObject.getFields("type") match {
      case Seq(JsString("file")) => value.convertTo[CanvasFileData]
      case Seq(JsString("text")) => value.convertTo[CanvasTextData]
      case Seq(JsString("link")) => value.convertTo[CanvasLinkData]
      case Seq(JsString("group")) => value.convertTo[CanvasGroupData]
      case _ => deserializationError("Unknown type in AllCanvasNodeData")
    }
  }

  implicit val canvasDataFormat: RootJsonFormat[CanvasData] = jsonFormat2(CanvasData)
}
