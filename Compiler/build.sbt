ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Compiler"
  )

libraryDependencies += "org.scala-lang.modules" % "scala-xml" % "2.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
coverageEnabled := true