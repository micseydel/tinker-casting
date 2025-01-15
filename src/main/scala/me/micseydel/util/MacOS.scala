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
}
