//ThisBuild / version := "0.1.0-SNAPSHOT"
val scala3Version = "3.3.1"
//ThisBuild / scalaVersion := "3.3.1"

// lazy val root = (project in file("."))
//   .settings(
//     name := "Compiler"
//   )

lazy val root = project
  .in(file("."))
  .settings(
    name := "FuncLog",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )

// //libraryDependencies += "org.scala-lang.modules" % "scala-xml" % "2.2.0"
// libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
// ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
// coverageEnabled := true