import sbt._

ThisBuild / scalaVersion := "2.13.6"

lazy val dependencies = List("org.scalatest" %% "scalatest" % "3.2.10")

lazy val root = project
  .in(file("."))
  .settings(
    name := "markov-movie-critic",
    version := "1",
    Compile / mainClass := Some("MarkovMovieCritic"),
    libraryDependencies ++= dependencies
  )
