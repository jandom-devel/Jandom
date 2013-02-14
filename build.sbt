// import assembly plugin setting

import AssemblyKeys._

assemblySettings

test in assembly := {}  // skip tests in assembly

name := "Jandom"

version := "0.1.1"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
  "org.rogach" %% "scallop" % "0.8.0"
)

resolvers ++= Seq(
   "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

fork  := true
