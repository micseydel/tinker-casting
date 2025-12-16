package me.micseydel.app

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.gson.GsonFactory
import com.google.api.services.slides.v1.Slides
import com.google.api.services.slides.v1.model.{BatchUpdatePresentationRequest, DeleteTextRequest, Dimension, InsertTextRequest, Page, Range, Request, TextStyle, UpdateTextStyleRequest}
import me.micseydel.actor.google.GoogleAuthManager.GoogleApplicationName
import me.micseydel.dsl.Tinker.Ability
import me.micseydel.dsl.tinkerer.NoteMakingTinkerer
import me.micseydel.dsl.{Operator, Tinker, TinkerColor, TinkerContext}

import scala.jdk.CollectionConverters.*


object GoogleSlideUpdater {
  sealed trait Message

  final case class SaveSlideInfoToNote(presentationId: String) extends Message

  case class Replacement(objectId: String, newText: String, fontSize: Int, italics: Boolean)
  final case class ReplaceText(presentationId: String, replacements: List[Replacement]) extends Message

  def apply(credential: Credential)(implicit Tinker: Tinker): Ability[Message] = NoteMakingTinkerer("Slides Tinkering", TinkerColor.random(), "🛝") { (context, noteRef) =>
    implicit val tc: TinkerContext[?] = context

    context.system.operator !! Operator.RegisterGoogleSlides(context.self)

    val service = new Slides.Builder(GoogleNetHttpTransport.newTrustedTransport, GsonFactory.getDefaultInstance, credential)
      .setApplicationName(GoogleApplicationName)
      .build

    Tinker.receiveMessage {
      case ReplaceText(presentationId, replacements) =>
        val requests = replacements.flatMap {
          case Replacement(objectId, newText, fontSize, italics) => replacementRequests(objectId, newText, fontSize, italics)
        }

        val batchUpdateRequest = new BatchUpdatePresentationRequest()
          .setRequests(requests.asJava)

        // FIXME: move into a Future
        service.presentations
          .batchUpdate(presentationId, batchUpdateRequest)
          .execute()
        context.self !! SaveSlideInfoToNote(presentationId)

        Tinker.steadily

      case SaveSlideInfoToNote(presentationId) =>
        val response = service.presentations.get(presentationId).execute
        val slides: List[Page] = response.getSlides.asScala.toList
        noteRef.setMarkdown(s"- Generated: ${context.system.clock.now()}\n\n" + slides.zipWithIndex.map { case (page, index) =>
          s"# Slide $index\n\n```json\n$page\n```"
        }.mkString("\n\n"))
        Tinker.steadily
    }
  }


  private def replacementRequests(objectId: String, newText: String, fontSize: Int, italics: Boolean): List[Request] = {
    val deleteTextRequest = new Request()
      .setDeleteText(
        new DeleteTextRequest()
          .setObjectId(objectId)
          .setTextRange(
            new Range()
              .setType("ALL")
          )
      )

    val insertTextRequest = new Request()
      .setInsertText(
        new InsertTextRequest()
          .setObjectId(objectId)
          .setInsertionIndex(0)
          .setText(newText)
      )

    val style = new Request().setUpdateTextStyle(
      new UpdateTextStyleRequest()
        .setObjectId(objectId)
        .setStyle(
          new TextStyle()
//            .setFontFamily("Roboto")
            .setFontSize(
              new Dimension()
                .setMagnitude(fontSize)
                .setUnit("PT")
            )
            .setItalic(italics)
//            .setBold(true)
        )
        .setTextRange(new Range().setType("ALL"))
//        .setFields("fontFamily,fontSize,bold")
        .setFields("fontSize,italic")
    )

    List(deleteTextRequest, insertTextRequest, style)
  }
}

