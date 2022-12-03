val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Advent of Code Scala",
    version := "1.0.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
  )
