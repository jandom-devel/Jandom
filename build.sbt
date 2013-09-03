import EclipseKeys._

lazy val Jandom = project in file("core")

lazy val JandomExtended = project in file("extended") dependsOn Jandom % "compile->compile;test->test"

lazy val root = project in file(".") aggregate (Jandom, JandomExtended) 

version in ThisBuild := "0.1.2-SNAPSHOT"

scalaVersion in ThisBuild := "2.10.2"

executionEnvironment in ThisBuild := Some(EclipseExecutionEnvironment.JavaSE17)

// The following settings are needed to correctly specify PPL native libraries at runtime

fork in ThisBuild := true

javaOptions in ThisBuild += "-Djava.library.path="+Process("ppl-config -l").lines.head+"/ppl"

// This delegates the root run task to the run task in JandomCore

run <<= run in ("Jandom", Compile)

