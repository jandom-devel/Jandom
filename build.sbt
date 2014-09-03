import EclipseKeys._

lazy val Jandom = project in file("core")

lazy val JandomExtended = project in file("extended") dependsOn Jandom % "compile->compile;test->test"

lazy val root = project in file(".") aggregate (Jandom, JandomExtended) 

version in ThisBuild := "0.1.3-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.2"

executionEnvironment in ThisBuild := Some(EclipseExecutionEnvironment.JavaSE17)

fork in ThisBuild := true

val optionalPPLPathName = try {
    val PPLPathName = Process("ppl-config -l").lines.head+"/ppl/ppl_java.jar"
    if (file(PPLPathName).exists) Some(PPLPathName) else None
  } catch {
    case _ : Exception => None 
  }

pplJar in ThisBuild := optionalPPLPathName

// This delegates the root run task to the run task in JandomCore

run <<= run in ("Jandom", Compile)

