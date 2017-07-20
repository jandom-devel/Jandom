//*** Declare projects

val Jandom = project in file("core") enablePlugins(BuildInfoPlugin)

val JandomExtended = project in file("extended")  dependsOn Jandom % "compile->compile;test->test"

val root = project in file(".")  aggregate (Jandom, JandomExtended) 

// This delegates the root run task to the run task in the Jandom project

run <<= run in ("Jandom", Compile)

run in Test <<= run in ("Jandom", Test)

// Add a new benchmark configuration...
// val Bench = config("bench") extend(Test)
// val root = project in file(".") configs(Bench) settings( inConfig(Bench) (Defaults.testSettings):_*) aggregate (Jandom, JandomExtended) 

//*** Scala configuration

scalaVersion in ThisBuild := "2.11.8"

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature", "-Xlint", "-Xlint:-delayedinit-select", "-Xlint:-missing-interpolator")

fork in ThisBuild := true

//*** Resolvers

resolvers in ThisBuild ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Soot snapshot" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-snapshot/",
  "Soot release" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-release/"
)

//*** Detect PPL

val optionalPPLPathName = try {
    val PPLPathName = Process("ppl-config -l").lines.head+"/ppl/ppl_java.jar"
    if (file(PPLPathName).exists) Some(PPLPathName) else None
  } catch {
    case _ : Exception => None 
  }

pplJar in ThisBuild := optionalPPLPathName

// for removing warnings when Breeze does not find native libraries
//
// javaOptions in ThisBuild ++= Seq("-Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.F2jBLAS",
//   "-Dcom.github.fommil.netlib.LAPACK=com.github.fommil.netlib.F2jLAPACK",
//   "-Dcom.github.fommil.netlib.ARPACK=com.github.fommil.netlib.F2jARPACK")

// Metadata

name in ThisBuild := "Jandom"

version in ThisBuild := "0.1.3-SNAPSHOT"

description in ThisBuild := "A static analyzer based on abstract interpretation"

organization in ThisBuild := "it.unich.jandom"

licenses in ThisBuild := Seq("LGPL-3.0" -> url("https://opensource.org/licenses/LGPL-3.0"))

homepage in ThisBuild := Some(url("https://github.com/jandom-devel/Jandom"))

startYear in ThisBuild := Some(2011)

developers := List(
  new Developer(
    "amato",
    "Gianluca Amato", "gianluca.amato@unich.it",
    url("http://www.sci.unich.it/~amato/")
  ),
  new Developer(
    "scozzari",
    "Francesca Scozzari", "francesca.scozzari@unich.it",
    url("http://www.sci.unich.it/~scozzari/")
  )
)

scmInfo := Some(new ScmInfo(
  url("https://github.com/jandom-devel/Jandom"),
  "scm:git:https://github.com/jandom-devel/Jandom.git",
  Some("scm:git:https://github.com/jandom-devel/Jandom.git")
))
