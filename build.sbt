import Dependencies._

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "bril-scala",
    libraryDependencies += munit % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
