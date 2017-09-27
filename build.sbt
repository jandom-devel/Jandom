import CustomKeys._

//*** Declare projects

lazy val JandomCore = project in file("core") enablePlugins(BuildInfoPlugin)

lazy val JandomExtended = project in file("extended") dependsOn JandomCore % "compile->compile;test->test"

lazy val Jandom = project in file(".") aggregate (JandomCore, JandomExtended) 

//*** This delegates the Jandom run task to execute the run task in the Jandom sub-projects

aggregate in assembly := false

assembly := (assembly in JandomExtended).value

run := (run in JandomCore in Compile).evaluated

run in Test := (run in JandomCore in Test).evaluated

run in Jmh := (run in JandomExtended in Jmh).evaluated

//*** Do not update snapshots every time

updateOptions in ThisBuild := updateOptions.value.withLatestSnapshots(false)

//*** Scala configuration

scalaVersion in ThisBuild := "2.12.3"

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint:_,-missing-interpolator", "-Ywarn-unused:-implicits")

fork in ThisBuild := true

//*** Resolvers

resolvers in ThisBuild ++= Seq (
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Soot snapshot" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-snapshot/",
  "Soot release" at
   "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-release/"
)

//*** Custom keys

pplJar in ThisBuild := {
  try {
    val PPLPathName = scala.sys.process.Process("ppl-config -l").lineStream.head+"/ppl/ppl_java.jar"
    if (file(PPLPathName).exists) Some(PPLPathName) else None
  } catch {
    case _ : Exception => None 
  }
}

gitHeadCommitSHA in ThisBuild := scala.sys.process.Process("git rev-parse HEAD").lineStream.head

//*** Eclipse plugin

// unfortunately, it is not possible to choose the compiler version with the eclipse plugin.

EclipseKeys.eclipseOutput := Some("target.eclipse")

//*** Metadata for the build

name in ThisBuild := "Jandom"

version in ThisBuild := "0.1.3-SNAPSHOT"

description in ThisBuild := "A static analyzer based on abstract interpretation"

organization in ThisBuild := "it.unich.jandom"

licenses in ThisBuild := Seq("LGPL-3.0" -> url("https://opensource.org/licenses/LGPL-3.0"))

homepage in ThisBuild := Some(url("https://github.com/jandom-devel/Jandom"))

startYear in ThisBuild := Some(2011)

developers in ThisBuild := List(
  Developer(
    "amato",
    "Gianluca Amato", "gianluca.amato@unich.it",
    url("http://www.sci.unich.it/~amato/")
  ),
  Developer(
    "scozzari",
    "Francesca Scozzari", "francesca.scozzari@unich.it",
    url("http://www.sci.unich.it/~scozzari/")
  )
)

scmInfo in ThisBuild := Some(ScmInfo(
  url("https://github.com/jandom-devel/Jandom"),
  "scm:git:https://github.com/jandom-devel/Jandom.git",
  Some("scm:git:https://github.com/jandom-devel/Jandom.git")
))
