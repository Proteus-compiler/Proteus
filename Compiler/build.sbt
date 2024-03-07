ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Compiler"
  )

libraryDependencies += "junit" % "junit" % "4.13.2" % "test"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
