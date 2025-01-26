package me.micseydel.util

import cats.data.Validated
import me.micseydel.NoOp

import java.io.{File, FileNotFoundException, FileWriter, IOException, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try, Using}

object FileSystemUtil {
  def validateDirectory(path: Path): Validated[String, Path] = {
    if (!Files.exists(path)) {
      Validated.invalid(s"Error: Root path $path does not exist.")
    } else if (!Files.isDirectory(path)) {
      Validated.invalid(s"Error: Root path $path is not a directory.")
    } else {
      Validated.valid(path)
    }
  }

  def validateDirectory(pathStr: String): Validated[String, Path] = {
    Try(Paths.get(pathStr)) match {
      case Success(path) =>
        validateDirectory(path)
      case Failure(ex) =>
        Validated.invalid(s"Error: Invalid path: $pathStr, Exception: ${ex.getMessage}")
    }
  }

  def getPathLines(path: Path): List[String] = {
    Using.resource(Source.fromFile(path.toString))(source => source.getLines().toList)
  }

  def getPathContents(path: Path): String = {
    Using.resource(Source.fromFile(path.toString))(_.mkString)
  }

  def getPathBytes(path: Path): Array[Byte] = {
    Files.readAllBytes(path)
  }

  def writeToPath(path: Path, string: String): NoOp.type = {
    val dir = path.toFile.getParentFile()
    if (!dir.exists && !dir.mkdirs()) throw new IOException(s"Path $path does not exist and creation failed")

    Using.resource(new PrintWriter(path.toString)) { writer =>
      writer.write(string)
    }
    NoOp
  }

  def appendToPath(path: Path, lines: List[String]): NoOp.type = {
    Using.resource(new PrintWriter(new FileWriter(path.toString, true))) { writer =>
      lines.foreach(writer.println)
    }
    NoOp
  }

  def appendToPath(path: Path, content: String): NoOp.type = {
    Using.resource(new PrintWriter(new FileWriter(path.toString, true))) { writer =>
      writer.println(content)
    }
    NoOp
  }

  def pathIsANonEmptyFile(path: String): Boolean = {
    try {
      new File(path).length() > 0
    } catch {
      case _: FileNotFoundException =>
        false
    }
  }
}
