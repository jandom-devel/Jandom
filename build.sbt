name := "Jandom"

version := "0.1.1"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.9" % "test",
  "org.scalala" %% "scalala" % "1.0.0.RC3-SNAPSHOT"
)

fork  := true

