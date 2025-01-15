package me.micseydel.prototyping

import me.micseydel.actor.AudioNoteCapturer.AcceptableFileExts
import spray.json._

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Using

object AudioNoteGenerator extends DefaultJsonProtocol {
  case class Segment(start: Float, text: String)

  implicit val segmentFormat: JsonFormat[Segment] = jsonFormat2(Segment)

  def genAudioPaths(inputDir: String): List[String] = {
    val files = new File(inputDir).listFiles
    files.filter(f => f.getName.toLowerCase.split("\\.").exists(AcceptableFileExts.contains)).map(_.getName).toList
  }

  def getSegments(path: String, inputDir: String): List[Segment] = {
    val jsonStr = Using(Source.fromFile(s"$inputDir/$path.json"))(_.mkString).getOrElse("")
    jsonStr.parseJson.convertTo[List[Segment]]
  }

  def generateBookmarks(path: String, inputDir: String): List[Map[String, String]] = {
    getSegments(path, inputDir).map { segment =>
      Map("start" -> segment.start.toString, "text" -> segment.text)
    }
  }

  def floatToHourMinuteSecond(f: Float): String = {
    val i = f.toInt
    f"${i / 3600}%02d:${(i / 60) % 60}%02d:${i % 60}%02d"
  }

  def generateFormattedBookmarks(path: String, inputDir: String): String = {
    generateBookmarks(path, inputDir).map { bookmark =>
      s"${floatToHourMinuteSecond(bookmark("start").toFloat)} --- ${bookmark("text")}"
    }.mkString("\n")
  }

  def generateTextSegments(path: String, inputDir: String): String = {
    getSegments(path, inputDir).map(segment => s"- ${segment.text}").mkString("\n")
  }

  def genWavWrapper(path: String, inputDir: String): String = {
    s"""|
        |---
        |tags:
        |- audionote
        |---
        |
        |# Segments
        |
        |${generateTextSegments(path, inputDir)}
        |
        |# Audio
        |
        |```audio-player
        |[[${path}]]
        |${generateFormattedBookmarks(path, inputDir)}
        |```
        |""".stripMargin
  }

  def main(args: Array[String]): Unit = {
    val inputDir = args(0)
    val outDir = args(1)

    val wavPaths = genAudioPaths(inputDir)

    if (!Files.isDirectory(Paths.get(outDir))) {
      sys.error(s"$outDir is not a directory")
    }

    println(s"Going to generate ${wavPaths.length} records from $inputDir outputting to $outDir")

    Using(new PrintWriter(new File(s"$outDir/Voice Recorder - voice memos experimentation.md"))) { writer =>
      wavPaths.foreach { wavPath =>
        val basePath = wavPath.dropRight(4)
        val wrapperPath = s"$basePath.md"
        Using(new PrintWriter(new File(s"$outDir/$wrapperPath"))) { subWriter =>
          subWriter.println(genWavWrapper(wavPath, inputDir))
        }
        Files.copy(Paths.get(s"$inputDir/$wavPath"), Paths.get(s"$outDir/$wavPath"))

        writer.println(s"- [ ] [[${basePath}#Segments]]")
        getSegments(wavPath, inputDir).foreach { segment =>
          writer.println(s"    - [ ] ${segment.text}")
        }
      }
    }
  }
}
