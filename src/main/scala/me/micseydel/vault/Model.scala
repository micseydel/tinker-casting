package me.micseydel.vault

import cats.data.NonEmptyList
import me.micseydel.dsl.cast.Gossiper
import me.micseydel.dsl.cast.Gossiper.{SubmitVote, Vote}
import me.micseydel.dsl.{SpiritRef, TinkerClock}
import org.yaml.snakeyaml.Yaml
import spray.json.{DefaultJsonProtocol, JsonFormat, RootJsonFormat}

import java.util
import scala.jdk.javaapi.CollectionConverters
import scala.util.{Success, Try}

object Model

// https://help.obsidian.md/Linking+notes+and+files/Internal+links#Link+to+a+block+in+a+note
sealed trait LinkId {
  def asString: String

  def wikiLinkWithAlias(alias: String): String = s"[[$asString|$alias]]"
  override def toString: String = s"[[$asString]]"
}


case class NoteId(id: String
                  // FIXME: support for subdirectories?
                  //                  , subdirectory: String
                 ) extends LinkId {
  override def asString: String = id

  def heading(heading: String): HeadingId = HeadingId(heading, this)

  def vote(confidence: Either[Double, Option[Boolean]], voter: SpiritRef[NonEmptyList[Vote]], comments: Option[String])(implicit clock: TinkerClock): Vote =
    Vote(this, confidence, voter, clock.now(), comments)

  def voteConfidently(confidence: Option[Boolean], voter: SpiritRef[NonEmptyList[Vote]], comments: Option[String])(implicit clock: TinkerClock): SubmitVote =
    Gossiper.SubmitVote(Vote(this, Right(confidence), voter, clock.now(), comments))

  def voteMeasuredly(confidence: Double, voter: SpiritRef[NonEmptyList[Vote]], comments: Option[String])(implicit clock: TinkerClock): SubmitVote =
    Gossiper.SubmitVote(Vote(this, Left(confidence), voter, clock.now(), comments))
}

object LinkIdJsonProtocol extends DefaultJsonProtocol {
  implicit def noteIdFormat: JsonFormat[NoteId] = jsonFormat1(NoteId)
}

case class HeadingId (heading: String, noteId: NoteId) extends LinkId {
  override def asString: String = s"${noteId.id}#$heading"
}

case class Note(
                 markdown: String,
                 // FIXME https://github.com/jcazevedo/moultingyaml
                 maybeFrontmatter: Option[String]
               ) {
  def raw: String = {
    maybeFrontmatter match {
      case Some(yaml) =>
        s"""---
           |$yaml
           |---
           |$markdown
           |""".stripMargin
      case None =>
        markdown
    }
  }

  def yamlFrontMatter: Try[Map[String, Any]] = maybeFrontmatter match {
    case Some(frontmatter) =>
      Try {
        val Yaml = new Yaml() // not thread safe
        val javaMap: java.util.Map[String, Any] = Yaml.load(frontmatter)
        val scalaMap: Map[String, Any] = CollectionConverters.asScala(javaMap).toMap
        scalaMap
      }
    case None =>
      Success(Map.empty)
  }
}

object Note {

  def apply(markdown: String, frontmatter: Map[String, Object]): Note  = {
    val Yaml = new Yaml()
    // workaround for Yaml library
    val asJava: util.Map[String, Object] = new java.util.HashMap[String, Object]()
    for ((key, value) <- frontmatter) {
      asJava.put(key, value)
    }

    Note(markdown, Some(Yaml.dump(asJava)))
  }
}

case class SpiritId private[vault](id: String)

object SpiritId {
  def apply(clazz: Class[_]): SpiritId = {
    // e.g. me.micseydel.vault.VaultKeeper$
    SpiritId(clazz.getName)
  }

  object SpiritIdJsonFormatter extends DefaultJsonProtocol {
    implicit val spiritIDJsonFormat: RootJsonFormat[SpiritId] = jsonFormat1(SpiritId.apply(_: String))
  }
}

