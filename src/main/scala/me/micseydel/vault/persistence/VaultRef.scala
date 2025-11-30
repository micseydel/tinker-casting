package me.micseydel.vault.persistence

import me.micseydel.NoOp
import me.micseydel.dsl.TinkerContext
import me.micseydel.util.FileSystemUtil
import me.micseydel.vault.persistence.NoteRef.{Contents, FileDoesNotExist, FileReadResult}
import me.micseydel.vault.{Note, NoteId, VaultPath}
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import spray.json._

import java.io.FileNotFoundException
import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

sealed trait VaultRef

abstract class NoteRef(val noteId: NoteId, val subdirectory: Option[String]) extends VaultRef {
  def readRaw(): Try[String]

  def readRawAsync()(implicit executionContext: ExecutionContext): Future[String]

  def setRaw(contents: String): Try[NoOp.type]

  def append(contents: String): Try[NoOp.type]

  def appendOrOption(contents: String): Option[Throwable] = append(contents).failed.toOption

  // helpers

  def appendOrThrow(contents: String): Unit = appendOrOption(contents) match {
    case Some(exception) => throw exception
    case None =>
  }

  def appendLine(line: String): Option[Throwable] = appendOrOption(line + "\n")
  def appendLine2(line: String): Option[Throwable] = appendOrOption(line)

  def setTo(note: Note): Try[Note] = {
    setRaw(note.raw).map(_ => note)
  }

  def read(): Try[Note] = {
    readRaw().map(rawToNote)
  }

  private def rawToNote(raw: String): Note = {
    if (raw.startsWith("---\n")) {
      // FIXME: low probability of the string below appearing in a YAML string
      raw.drop(4).split("\n---\n", 2) match {
        case Array(yaml, markdown) =>
          Note(markdown, Some(yaml))
        case _ =>
          Note(raw, None)
      }
    } else {
      Note(raw, None)
    }
  }

  def readMarkdownSafer(): FileReadResult = {
    readMarkdown() match {
      case Failure(_: FileNotFoundException) => FileDoesNotExist
      case tried => Contents(tried)
    }
  }

  def readMarkdown(): Try[String] = {
    read().map(_.markdown)
  }

  def readMarkdownAsync()(implicit executionContext: ExecutionContext): Future[String] =
    readRawAsync().map(rawToNote).map(_.markdown)

  def readNoteAsync()(implicit executionContext: ExecutionContext): Future[Note] =
    readRawAsync().map(rawToNote)

  def readNote(): Try[Note] =
    readRaw().map(rawToNote)

  def readMarkdownAndFrontmatter(): Try[(String, Map[String, Any])] =
    readNote().flatMap { note =>
      note.yamlFrontMatter.map(frontmatter => (note.markdown, frontmatter))
    }

  def upsert(f: Note => Note): Try[Note] = {
    read()
      .recoverWith {
        case _: FileNotFoundException =>
          Success(Note("", None))
      }
      .map(f)
      .flatMap(setTo)
  }

  def setMarkdown(markdown: String): Try[NoOp.type] = {
    //    println(s"Setting markdown to size ${markdown.length} for $noteId")
    upsert(_.copy(markdown = markdown)).map(_ => NoOp)
  }

  def updateMarkdown(f: String => String): Try[Note] = {
    upsert {
      case Note(markdown, frontmatter) =>
        Note(f(markdown), frontmatter)
    }
  }

  def setFrontMatter(frontmatter: AnyRef): Try[Note] = {
    upsert {
      case Note(markdown, _) =>
        val options = new DumperOptions();
        options.setIndent(4);
        options.setPrettyFlow(true);
        // FIXME: I'm pretty sure this doesn't work
        options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
        val Yaml = new Yaml(options) // needs to be kept in its own thread
        Note(markdown, Some(Yaml.dump(frontmatter)))
    }
  }
}

object NoteRef {
  sealed trait FileReadResult
  case class Contents(s: Try[String]) extends FileReadResult
  case object FileDoesNotExist extends FileReadResult
}


class BasicNoteRef(override val noteId: NoteId, vaultRoot: VaultPath, subdirectory: Option[String]) extends NoteRef(noteId, subdirectory) {
  private[this] val NoteFileName = s"${noteId.id}.md"

  private[persistence] def notePath: Path = subdirectory match {
    case Some(path) =>
      vaultRoot.resolve(path).resolve(NoteFileName)
    case None =>
      vaultRoot.resolve(NoteFileName)
  }

  override def toString: String = s"NoteRef(${noteId.id})"

  def readRaw(): Try[String] = Try {
    FileSystemUtil.getPathContents(notePath)
  }

  def setRaw(contents: String): Try[NoOp.type] = {
    Try {
      FileSystemUtil.writeToPath(notePath, contents)
    }
  }

  override def append(contents: String): Try[NoOp.type] = {
    Try {
      FileSystemUtil.appendToPath(notePath, contents)
    }
  }

  /**
   * @return Nil when path does not exist
   */
  //noinspection AccessorLikeMethodIsEmptyParen
  def getLines(): Try[List[String]] = {
    readRaw()
      .map(_.split("\\n").toList)
      .recoverWith {
        case _: FileNotFoundException =>
          Success(Nil)
      }
  }

  override def readRawAsync()(implicit executionContext: ExecutionContext): Future[String] = Future {
    readRaw() match {
      case Failure(exception) => throw exception
      case Success(value) => value
    }
  }
}

sealed trait GenericJsonRef extends VaultRef

trait JsonRef extends GenericJsonRef {
  val filename: String

  def set(contents: String): Try[NoOp.type]

  def read(): Try[String]

  def append(contents: String): Try[NoOp.type]
}

class BasicJsonRef(val filename: String, jsonPath: Path) extends JsonRef {
  private val path = jsonPath.resolve(s"$filename.json")

  def set(contents: String): Try[NoOp.type] = {
    Try(FileSystemUtil.writeToPath(path, contents))
  }

  def read(): Try[String] = {
    Try(FileSystemUtil.getPathContents(path))
  }

  def append(contents: String): Try[NoOp.type] = {
    Try(FileSystemUtil.appendToPath(path, contents))
  }
}

trait TypedJsonRefT[T] extends GenericJsonRef {
  def set(contents: T): Try[NoOp.type]

  def read(): Try[T]

  def updateOrSetDefault(default: T)(updater: T => T): Try[T] = {
    read()
      .map(updater)
      .recoverWith {
        case _: FileNotFoundException => Success(default)
      }
      .flatMap { updated =>
        set(updated).map(_ => updated)
      }
  }
}

class TypedJsonRef[T](jsonRef: JsonRef)(implicit jsonFormat: JsonFormat[T]) extends TypedJsonRefT[T] {
  def set(contents: T): Try[NoOp.type] = {
    jsonRef.set(contents.toJson(jsonFormat).prettyPrint)
  }

  def read(): Try[T] = {
    jsonRef.read().map { contents =>
      contents.parseJson.convertTo[T](jsonFormat)
    }
  }
}

trait JsonlRefT[T] extends GenericJsonRef {
  def get(): Try[List[T]]

  def appendAndGet(t: T): Try[List[T]]

  def append(t: T): Try[NoOp.type]
}

class JsonlRef[T](jsonRef: JsonRef)(implicit jsonFormat: JsonFormat[T]) extends JsonlRefT[T] {
  override def get(): Try[List[T]] = getList()

  def appendAndGet(t: T): Try[List[T]] = {
    jsonRef.append(t.toJson(jsonFormat).compactPrint + "\n")
      .flatMap(_ => getList())
  }

  def append(t: T): Try[NoOp.type] = {
    jsonRef.append(t.toJson(jsonFormat).compactPrint + "\n")
  }

  private def getList(): Try[List[T]] = {
    jsonRef.read().map { contents =>
      contents
        .split("\n")
        .filter(_ != "") // FIXME: hack, should clean up the file generation
        .toList
        .map(_.parseJson.convertTo[T](jsonFormat))
    }.recoverWith {
      case _: FileNotFoundException =>
        Success(Nil)
    }
  }
}

