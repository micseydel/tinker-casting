// newer versions that have a license I don't want to use
//lazy val akkaHttpVersion = "10.5.0"
//lazy val akkaVersion    = "2.8.0"

// older versions, but...
lazy val akkaHttpVersion = "10.2.10"
lazy val akkaVersion    = "2.6.21"

lazy val additionalLibraryDependencies = Seq(
  "com.typesafe.akka" %% "akka-http"                % akkaHttpVersion,

//  "net.virtual-void" %%  "json-lenses" % "0.6.2",
//  "com.chuusai" %% "shapeless" % "2.3.12",
  "com.typesafe.akka" %% "akka-http-spray-json"     % akkaHttpVersion,

  "com.typesafe.akka" %% "akka-actor-typed"         % akkaVersion,
  "com.typesafe.akka" %% "akka-stream"              % akkaVersion,
  //      "com.typesafe.akka" %% "akka-stream-typed"        % akkaVersion,

  "org.slf4j" % "slf4j-api" % "2.0.5",
  "ch.qos.logback"    %  "logback-classic"          % "1.4.7",

  "org.typelevel"     %% "cats-core"                % "2.9.0",
  "net.jthink"        % "jaudiotagger"              % "3.0.1",

  "com.softwaremill.quicklens" %% "quicklens" % "1.8.10",

  // for mqtt
  "com.sandinh" %% "paho-akka" % "1.6.1",

  "org.yaml" % "snakeyaml" % "2.4",
//  "org.virtuslab" %% "scala-yaml" % "0.1.0",

  "com.google.api-client" % "google-api-client" % "2.7.2",
  "com.google.oauth-client" % "google-oauth-client-jetty" % "1.39.0",
  "com.google.apis" % "google-api-services-gmail" % "v1-rev20240520-2.0.0",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.18.3",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.18.3",

  // testing

  "com.typesafe.akka" %% "akka-http-testkit"         % akkaHttpVersion % Test,
  "com.typesafe.akka" %% "akka-actor-testkit-typed"  % akkaVersion     % Test,
  "org.scalatest"     %% "scalatest"                 % "3.2.17"        % Test,
  "org.scalamock"     %% "scalamock"                 % "5.1.0"         % Test
)

// Run in a separate JVM, to make sure sbt waits until all threads have
// finished before returning.
// If you want to keep the application running while executing other
// sbt tasks, consider https://github.com/spray/sbt-revolver/
fork := true

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization    := "com.example",
      scalaVersion    := "2.13.8"
    )),
    name := "akka-http-quickstart-scala",
    libraryDependencies ++= additionalLibraryDependencies
  )

// for `sbt "runMain me.micseydel.KaizenCasterApp"` to be able to use CLI
//connectInput / run := true

//connectInput in run := true // deprecated
run / connectInput := true

scalacOptions += "-Wconf:cat=other-match-analysis:error"
scalacOptions += "-deprecation"
scalacOptions += "-Xsource:3"

version := "0.2"


Docker / packageName := "dockerized-tinker-cast"
dockerBaseImage := "openjdk:8-jre-alpine"
//packageName in Docker := "dockerized-tinker-cast"
enablePlugins(
  // for generating an executable
  JavaAppPackaging,
  // for Bash in openjdk:8-jre-alpine
  AshScriptPlugin
)

// https://medium.com/free-code-camp/how-to-dockerise-a-scala-and-akka-http-application-the-easy-way-23310fc880fa
//import com.typesafe.sbt.packager.docker.{Cmd, ExecCmd}
//dockerCommands ++= Seq(
//  Cmd("USER", "root"),
//  ExecCmd("RUN", "apk", "add", "--no-cache", "bash")
//)
