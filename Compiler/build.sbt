ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "Compiler"
  )
