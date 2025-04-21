package me.micseydel.util

import scala.sys.process.{Process, ProcessIO}

object MacOS {
  def wifiTurnedOn: Either[String, Boolean] = {
    val WifiOn = UnderlyingCommandResult(0, "Wi-Fi Power (en0): On\n", "")
    val WifiOff = UnderlyingCommandResult(0, "Wi-Fi Power (en0): Off\n", "")
    runCommand(Seq("networksetup", "-getairportpower", "en0")) match {
      case WifiOn =>
        Right(true)
      case WifiOff =>
        Right(false)
      case other =>
        Left(s"Expected either $WifiOn or $WifiOff but got $other")
    }
  }
  
  case class UnderlyingCommandResult(exitCode: Int, output: String, error: String)
  def runCommand(command: Seq[String]): UnderlyingCommandResult = {
    // Prepare StringBuilder objects to capture output and errors
    val output = new StringBuilder
    val error = new StringBuilder

    // Define ProcessIO to capture output and errors
    val processIO = new ProcessIO(_ => (),
      stdout => scala.io.Source.fromInputStream(stdout).getLines().foreach(line => output.append(line).append("\n")),
      stderr => scala.io.Source.fromInputStream(stderr).getLines().foreach(line => error.append(line).append("\n")))

    // Execute the command
    val exitCode = Process(command).run(processIO).exitValue()

    UnderlyingCommandResult(exitCode, output.toString, error.toString)
  }

  def main(args: Array[String]): Unit = {
    val result = runCommand(Seq("say", "--voice=?"))
    val filtered = result.output.split("\n").filter(_.contains("en_US")).toList

    val pairs: List[(String, String)] = filtered.map { line =>
      line.split("en_US {4}#").map(_.strip).toList match {
        case List(name, sampleSentence) =>
          (name, sampleSentence)
        case other => throw new RuntimeException(s"Unexpected: $other")
      }
    }

    for ((name, sampleSentence) <- pairs) {
      val command = Seq("say", "-v", name, sampleSentence)
      println(s"Trying $command")
      runCommand(command)
    }
  }
}
