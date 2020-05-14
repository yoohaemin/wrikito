import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"


lazy val root = (project in file("root"))
  .settings(
    name := "mockito-like-with-cats",
    scalacOptions ++= Seq("-Xlint", "-Ymacro-annotations"),
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.typelevel" %% "cats-tagless-macros" % "0.11",
      "org.scala-lang" % "scala-reflect" % "2.13.1",
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
  ).dependsOn(macros)

lazy val macros = project.settings(
  name := "macros",
  scalacOptions ++= Seq("-Xlint", "-Ymacro-annotations", "-language:_"),
  libraryDependencies ++= Seq(
    scalaTest % Test,
    "org.typelevel" %% "cats-tagless-macros" % "0.11",
    "org.scala-lang" % "scala-reflect" % "2.13.1",
  ),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

