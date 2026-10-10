ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "io.canopyrisk"
ThisBuild / version := "0.1.0-SNAPSHOT"

val catsEffectVersion = "3.6.3"
val fs2Version = "3.12.2"
val http4sVersion = "0.23.33"
val circeVersion = "0.14.15"
val declineVersion = "2.5.0"
val munitVersion = "1.2.1"
val munitCatsEffectVersion = "2.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "canopy-risk",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:all",
      "-Wvalue-discard",
      "-Werror",
      "-source:3.3"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "co.fs2" %% "fs2-core" % fs2Version,
      "co.fs2" %% "fs2-io" % fs2Version,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "com.monovore" %% "decline-effect" % declineVersion,
      "org.slf4j" % "slf4j-nop" % "2.0.17",
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.typelevel" %% "munit-cats-effect" % munitCatsEffectVersion % Test
    ),
    Compile / run / fork := true,
    Test / fork := true,
    Test / parallelExecution := false,
    assembly / mainClass := Some("canopy.Main"),
    assembly / assemblyJarName := "canopy-risk.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "versions", _*)              => MergeStrategy.first
      case PathList("META-INF", "MANIFEST.MF")               => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.lastOption.exists(_.endsWith(".SF")) =>
        MergeStrategy.discard
      case PathList("META-INF", "services", _*)              => MergeStrategy.concat
      case PathList("META-INF", _*)                          => MergeStrategy.discard
      case "module-info.class"                               => MergeStrategy.discard
      case x =>
        val old = (assembly / assemblyMergeStrategy).value
        old(x)
    }
  )
